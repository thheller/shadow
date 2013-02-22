(ns shadow.object
  (:require [shadow.dom :as dom]
            [clojure.string :as str]
            [clojure.data :as data]))

(defn console-friendly [a]
  (cond
   (nil? a) "nil"
   (string? a) a
   (number? a) a
   (satisfies? IPrintWithWriter a) (pr-str a)
   :else a
   ))

(defn log
  ([a1]
     (.log js/console (console-friendly a1)))
  ([a1 a2]
     (.log js/console (console-friendly a1) (console-friendly a2)))
  ([a1 a2 a3]
     (.log js/console (console-friendly a1) (console-friendly a2) (console-friendly a3)))
  ([a1 a2 a3 a4]
     (.log js/console (console-friendly a1) (console-friendly a2) (console-friendly a3) (console-friendly a4)))
  ([a1 a2 a3 a4 a5]
     (.log js/console (console-friendly a1) (console-friendly a2) (console-friendly a3) (console-friendly a4) (console-friendly a5)))
  ([a1 a2 a3 a4 a5 a6]
     (.log js/console (console-friendly a1) (console-friendly a2) (console-friendly a3) (console-friendly a4) (console-friendly a5) (console-friendly a6)))
  ([a1 a2 a3 a4 a5 a6 & more]
     (.log js/console (console-friendly a1) (console-friendly a2) (console-friendly a3) (console-friendly a4) (console-friendly a5) (console-friendly a6) "more:" (pr-str more))))

(def debug log)

(def info log)

(def warn log)

(def error log)

(def obj-id (atom 0))
(defn next-id []
  (swap! obj-id inc))

(def object-defs (atom {}))
(def behavior-fns (atom {}))

(def instances (atom {}))
(def instance-parent (atom {}))
(def instance-children (atom {}))

(def events (atom {}))

(defn define-event [event-name desc args]
  (swap! events assoc event-name {:id event-name
                                  :description desc
                                  :args args}))

(defn unmunge [s]
  (-> s
      (str/replace #"_DOT_" ".")
      (str/replace #"_" "-")
      ))

(defn get-func-signature [func]
  (let [name (.-name func)]
    (when (seq name)
      (let [[_ ns fn-name line :as x] (re-find #"fn_(.+)_SLASH_(.+)_(\d+)" name)]
        (if x
          (str (unmunge ns) "/" (unmunge fn-name) " line:" line)
          name)))
    ))

(extend-type js/Function
  IPrintWithWriter
  (-pr-writer [this writer opts]
    (if-let [sig (get-func-signature this)]
      (-write writer (str "#<function " sig ">"))
      (-write writer (str "#<" this ">"))
      )))

(define-event :init
  "object initialization"
  [])

(define-event :destroy
  "object descruction"
  [[:cause "cause of destruction, :parent means the parent was destroyed, :direct is default"]])

(define-event :dom-init
  "called after the dom was created, use it to put it into the document"
  [[:dom "the dom that was created"]])

(define-event :updated
  "called after obj/update! completed"
  [])

(defn behavior* [& args]
  (warn "defbehavior is gone" (first args)))

(defprotocol IObject
  (-id [this])
  (-type [this])
  (-data [this]))

(defn get-type-attr
  ([oref key] (get-in @object-defs [(-type oref) key]))
  ([oref key default] (get-in @object-defs [(-type oref) key] default)))


(defn notify! [oref ev & args]
  (when-not (contains? @events ev)
    (debug "triggering undefined notifiction" (pr-str ev) " with " (pr-str args)))

  (let [reactions-to-trigger (get-in oref [::reactions ev] (list))]
    ;; (debug "notify!" (-id oref) (-type oref) ev reactions-to-trigger args)
    (doseq [rfn reactions-to-trigger]
      (apply rfn (cons oref args)))
    ))

(defn update! [oref update-fn & args]
  (when-not (fn? update-fn)
    (throw (str "update! expects a fn as second arg, not " (pr-str update-fn))))

  (let [id (-id oref)
        data (-data oref)]
    (swap! data (fn [data] (apply update-fn data args)))
    (notify! oref :updated)
    ))

(defn ^:export get-dom [oref]
  (::dom oref))

(defn ^:export get-by-id [id]
  (get @instances id))

(defn ^:export get-from-dom [dom]
  (let [oid (dom/attr dom :data-oid)]
    (when-not oid
      (throw (ex-info "get-from-dom only works on nodes created via obj/create" {:dom dom})))

    (get-by-id (js/parseInt oid))
    ))

(defn ^:export get-object-data [oref]
  @(-data oref))

(defn ^:export get-parent [oref]
  (when-let [parent-id (get @instance-parent (-id oref))]
    (get @instances parent-id)
    ))

(defn ^:export get-children [parent]
  (let [parent-id (-id parent)
        child-ids (get @instance-children parent-id [])
        instances @instances]
    (vec (map #(get instances %) child-ids))
    ))

(defn get-children-of-type [oref type]
  (let [type-kw (if (keyword? type) type (-type type))]
    (filter #(= type-kw (-type %)) (get-children oref))
    ))

(defn- set-parent! [child parent]
  (let [child-id (-id child)
        parent-id (-id parent)]
    (swap! instance-parent assoc child-id parent-id)
    (swap! instance-children (fn [x]
                               (let [current (get x parent-id #{})]
                                 (assoc x parent-id (conj current child-id))
                                 )))
    true))

(defn dom-destroy [this cause]
  (if (= cause :parent)
    ;; no messin arround when parent is dead
    (dom/remove (::dom this))
    ;; custom removals are allowed when removing direct
    (if-let [custom-remove (get-type-attr this :dom-remove)]
      (custom-remove this (::dom this))
      (dom/remove (::dom this))
      )))

(defn destroy!
  ([oref]
     (destroy! oref :direct))
  ([oref cause]
     (let [obj-id (-id oref)
           parent-id (get @instance-parent obj-id)]

       ;; destroy children before destroying parent
       (doseq [child (get-children oref)]
         (destroy! child :parent))

       (notify! oref :destroy cause)
       (dom-destroy oref cause)

       (swap! instances dissoc obj-id)
       (swap! instance-parent dissoc obj-id)
       (when parent-id
         (swap! instance-children update-in [parent-id] disj obj-id)))))

(comment
  "should I create basically instance methods? doesnt really seam that useful"
  (defn call [obj handler & args]
    (let [handler-fn (get-type-attr obj handler)]
      (when-not handler-fn
        (throw (ex-info "handler not found in object" {:handler handler :obj obj})))
      (apply handler-fn (conj args obj))
      )))

(defn- bind-dom-events [oref dom dom-events]
  (when-not (zero? (rem (count dom-events) 2))
    (throw (ex-info "object defined invalid event" {:object-type (-type oref) :dom-events dom-events})))

  (doseq [[ev handler :as ev-def] (partition 2 dom-events)]
    (when (nil? handler)
      (throw (str "ev with nil handler " (pr-str ev))))

    (let [handler (if (keyword? handler)
                    (fn [this e]
                      (dom/ev-stop e)
                      (notify! this handler e))
                    handler)]
      (dom/on dom ev (fn dom-event-handler [e] (handler oref e))))))

(defn dom-init [this]
  (when-let [dom-fn (get-type-attr this :dom)]
    (let [dom-events (get-type-attr this :dom-events [])
          dom (dom/build (dom-fn this))]

      ;; js/Text cant have attr, but no object should ever have text as root node?
      (dom/set-attr dom :data-oid (-id this))

      (update! this assoc ::dom dom)
      (bind-dom-events this dom dom-events)
      dom
      )))

(defn- reaction-merge [result event handler]
  (let [current (get result event (list))]
    (assoc result event (conj current handler))))

(defn- merge-reactions [result behavior]
  (when-not (map? behavior)
    (throw (ex-info "behaviors must be maps" {:behavior behavior})))
  (reduce-kv reaction-merge result behavior))

(defn define [id & args]
  (when-not (even? (count args))
    (throw (str "invalid object definition " (str id) " args: "(pr-str args))))
  (when (contains? @object-defs id)
    (throw (str "object already defined " id)))

  (let [odef (apply hash-map args)

        reactions (merge-reactions {} (apply hash-map (:on odef [])))
        reactions (reduce merge-reactions
                          reactions
                          ;; reverse order seems more logical (aka apply in order they are given)
                          (reverse (get odef :behaviors [])))

        odef (assoc odef
               ::id id
               ::reactions reactions
               )]

    (swap! object-defs assoc id odef)))


(defn- merge-defaults [data type]
  (let [defaults (get-in @object-defs [type :defaults])]
    (cond
     (nil? defaults) data
     (map? defaults) (merge data defaults)
     (fn? defaults) (merge data (defaults))
     :else (throw (ex-info "invalid object defaults" {:defaults defaults :type type}))
     )))

(defn make-dom
  ([oref dom-key events-key]
     (make-dom oref dom-key events-key nil))
  ([oref dom-key events-key value]
      (let [dom-fn (if (keyword? dom-key) (get-type-attr oref dom-key) dom-key)
            events (if (keyword? events-key) (get-type-attr oref events-key []) events-key)
            dom (dom/build (dom-fn oref value))]
        (bind-dom-events oref dom events)
        dom
        )))


(deftype ObjectRef [id type data]
  IObject
  (-id [this] id)
  (-type [this] type)
  (-data [this] data)

  IEquiv
  (-equiv [this other]
    (and (instance? ObjectRef other)
         (= (-id this) (-id other))))

  IDeref
  (-deref [this] @data)

  IPrintWithWriter
  (-pr-writer [this writer opts]
    (-write writer (str "#<ObjectRef {:id " id ", :type " type ", :data " data "}>")))

  IWatchable
  (-notify-watches [this oldval newval]
    (-notify-watches data oldval newval))
  (-add-watch [this key f]
    (-add-watch data key f))
  (-remove-watch [this key]
    (-remove-watch data key))

  ILookup
  (-lookup [this k]
    (if (= :parent k)
      (get-parent this)
      (get @data k)))
  (-lookup [this k d]
    (if (= :parent k)
      (or (get-parent this) k)
      (get @data k d)))

  Object
  (toString [this]
    (pr-str this))

  dom/IElement
  (-to-dom [this] (::dom this))
  )

(defn create [type obj]
  (when-not (contains? @object-defs type)
    (throw (ex-info (str "cannot create unknown child type: " type) {:type type :obj obj})))
  (when-not (map? obj)
    (throw (ex-info "obj/create second arg must be a map" {:obj obj})))

  (let [oid (next-id)
        obj (-> obj
                (assoc ::object-id oid
                       ::reactions (get-in @object-defs [type ::reactions] {}))
                (merge-defaults type))
        parent (:parent obj)
        obj-atom (atom (dissoc obj :parent))
        oref (ObjectRef. oid type obj-atom)]

    ;; dont use oref before this
    (swap! instances assoc oid oref)

    (when parent
      (set-parent! oref parent))

    (notify! oref :init)

    (when-let [dom (dom-init oref)]
      (notify! oref :dom-init dom))

    oref
    ))

(defn add-reaction!
  ([oref ev handler-fn]
     (add-reaction! oref {ev handler-fn}))
  ([oref map]
     (update! oref update-in [::reactions] merge-reactions map)))

(defn bind-change [oref attr callback]
  (when-not (satisfies? IObject oref)
    (throw (ex-info "binding currently only supports shadow objects, other atoms might leak, may add later" {:oref oref :attr attr})))

  (let [attr (if (vector? attr) attr [attr])]
    (add-watch oref (gensym "bind-change")
               (fn [_ _ old new]
                 (let [ov (get-in old attr)
                       nv (get-in new attr)]
                   (when-not (= ov nv)
                     (callback ov nv)))))))

(defn bind
  "[oref attr node-gen] produces a node via (node-gen new-value)
   watches obj for changes and replaces the generated node on change (node-gen defaults to str)"
  ([oref attr] (bind oref attr str))
  ([oref attr node-gen]
     (let [attr (if (vector? attr) attr [attr])
           node-get #(dom/build (node-gen %))
           node (atom (node-get (get-in oref attr)))
           bind-key (gensym "bind")]

       (bind-change oref attr
                    (fn [old new]
                      (let [new-node (node-get new)
                            current-node @node]
                        (dom/replace-node current-node new-node)
                        (reset! node new-node))))

       @node)
     ))

(define ::bound-collection
  :dom ::coll-root)

(defn coll-destroy-children [children c diff]
  ;; whats more efficient in the DOM, drop head or tail?
  ;; diff is neg!
  (doseq [obj (subvec children (+ c diff) c)]
    (destroy! obj))
  (subvec children 0 (+ c diff)))

(defn bind-children
  ([node oref attr item-type item-key]
     (bind-children node oref attr item-type item-key #(map-indexed vector %)))
  ([node oref attr item-type item-key coll-transform]
     (let [attr (if (vector? attr) attr [attr])
           coll-obj (create ::bound-collection {:parent oref
                                                ::coll-root node
                                                ::coll-path attr
                                                ::coll-item-key item-key})

           coll-get #(vec (coll-transform (get-in % attr)))

           make-item-fn (fn [[key val]]
                          (let [obj (create item-type {:parent oref
                                                       ::coll-path attr
                                                       ::coll-key key
                                                       ::coll-item-key item-key
                                                       item-key val})]

                            (bind-change obj item-key
                                       (fn [_ new]
                                         ;; FIXME: this wont work for sets, only vec and map

                                         ;; TODO: updating here will also trigger the watch below
                                         ;; which will do a whole lot of work for nothing?
                                         ;; remove-watch/add-watch is not an option since you dont get the function
                                         ;; via remove-watch
                                         (update! oref assoc-in (conj attr key) new)
                                         ))
                            obj
                            ))

           ]

       (doseq [item (coll-get @oref)]
         (dom/append coll-obj (make-item-fn item)))

       (bind-change oref attr
                    (fn [old new]
                      (let [children (vec (map get-from-dom (dom/children coll-obj)))
                            new-coll (coll-get new)
                            count-children (count children)
                            count-new (count new-coll)
                            diff (- count-new count-children)

                            ;; exit lost children
                            children (if (neg? diff)
                                       (coll-destroy-children children count-children diff)
                                       children)
                            count-children (min count-new count-children)]

                        ;; update current
                        (dotimes [idx count-children]
                          (let [cc (nth children idx)
                                ckey (::coll-key cc)
                                cval (get cc item-key)
                                [nkey nval] (nth new-coll idx)]

                            ;; only update when something changes
                            (when-not (and (= ckey nkey) (= cval nval))
                              (update! cc assoc item-key nval ::coll-key nkey)
                              )))

                        ;; enter new
                        (when (pos? diff)
                          (doseq [item (subvec new-coll count-children count-new)]
                            (dom/append coll-obj (make-item-fn item))))

                        (notify! oref :bind-children-update)
                        )))

       coll-obj)))

(defn remove-from-vector [coll key]
  (let [c (count coll)]
    (cond
     (= 0 key) (subvec coll 1)
     (= (dec c) key) (subvec coll 0 (dec c))
     :else ;; item in teh middle
     (vec (concat (subvec coll 0 key)
                  (subvec coll (inc key) c))))
    ))

(defn remove-item-from-coll [coll key]
  (cond
   (satisfies? IVector coll)
   (remove-from-vector coll key)
   (satisfies? IMap coll)
   (dissoc coll key)
   (satisfies? ISet coll)
   (disj coll key)
   :else (throw "unknown coll type")
   ))


(defn remove-in-parent! [oref]
  (let [parent (get-parent oref)
        key (::coll-key oref)
        path (::coll-path oref)]

    (when-not (and key path)
      (throw (ex-info "remove-in-parent! should only be called from items created via obj/bind-children" {:oref oref})))

    (.log js/console "remove in parent" (get-in parent path))
    (update! parent update-in path remove-item-from-coll key)
    ))

(defn inspect! [oref]
  (info "inspect!" (-id oref) (str (-type oref)) (clj->js @(-data oref))))

(defn ^:export dump! []
  (info "--------------- LIVE OBJECTS ------------")
  (doseq [[id oref] (seq @instances)]
    (info "dump" id (pr-str (-type oref)) @(-data oref)))
  (info "--------------- //LIVE OBJECTS ------------")
  )
