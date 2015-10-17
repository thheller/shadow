(ns shadow.object
  (:refer-clojure :exclude (tree-seq))
  (:require [shadow.dom :as dom]
            [cljs.core.async :as async]
            [clojure.string :as str]
            [clojure.data :as data]
            [cljs.core.async.impl.protocols :as async-impl]
            [shadow.util :as util]
            ))

(def console-friendly util/console-friendly)

(def log util/log)
(def debug util/log)
(def info util/log)
(def warn util/log)
(def error util/log)

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

(define-event :init
  "object initialization"
  [])

(define-event :destroy
  "object descruction"
  [[:cause "cause of destruction, :parent means the parent was destroyed, :direct is default"]])

(define-event :dom/init
  "called after the dom was created but has not entered the document yet"
  [[:dom "the dom that was created"]])

(define-event :dom/entered
  "should be called whenever a dom node is added to the document, since that
   is not in control of this library its up to you to call this
   use (so/notify-tree! your-obj :dom/entered) to notify the node and every child you created"
  [])

(define-event :bind-children-update
  "need to rethink this"
  [])

(defprotocol IObject
  (-id [this])
  (-type [this])
  (-data [this])
  (-update [this update-fn] "update and notify watches")
  (-destroy! [this cause]))

(defn get-type [this]
  (-type this))

(defn get-type-attr
  ([oref key]
     (if (nil? oref)
       nil
       (let [type-id (if (keyword? oref) oref (-type oref))]
         (get-in @object-defs [type-id key]))))
  ([oref key default]
     (if (nil? oref)
       default
       (let [type-id (if (keyword? oref) oref (-type oref))]
         (get-in @object-defs [type-id key] default)))))

(defn ^:export get-dom [oref]
  (::dom oref))

(defn ^:export get-by-id [id]
  (get @instances id))

(defn ^:export get-from-dom [dom]
  (let [oid (dom/data dom :oid)]
    (when oid
      (get-by-id (js/parseInt oid)))
    ))

(defn is-object? [obj-or-dom]
  (or (satisfies? IObject obj-or-dom)
      (get-from-dom obj-or-dom)))

(defn equal? [obj obj-or-dom]
  (assert (satisfies? IObject obj) "can only test objects")
  (if (satisfies? IObject obj-or-dom)
    (= (-id obj) (-id obj-or-dom))
    (= (-id obj) (when-let [oid (dom/data obj-or-dom :oid)]
                   (js/parseInt oid)))))

(defn ^:export get-parent [oref]
  (when-let [parent-id (get @instance-parent (-id oref))]
    (get @instances parent-id)
    ))

(defn ^:export get-parent-of-type [oref parent-type]
  (loop [parent (:parent oref)]
    (when parent
      (if (= (-type parent) parent-type)
        parent
        (recur (:parent parent))))))

;; FIXME: would be nice if these were in dom order
;; but since children arent always direct dom children
;; this would be kinda costly I guess
(defn ^:export get-children [parent]
  (let [parent-id (-id parent)
        child-ids (get @instance-children parent-id [])
        instances @instances]
    (vec (map #(get instances %) child-ids))
    ))

(defn ^:export tree-seq
  ([root]
     (tree-seq root (fn [node] true)))
  ([root branch?]
     (cljs.core/tree-seq branch? get-children root)))

(defn get-children-of-type [oref type]
  (let [type-kw (if (keyword? type) type (-type type))]
    (filter #(= type-kw (-type %)) (get-children oref))
    ))

(defn get-siblings
  "basically (get-children (:parent this))"
  [{:keys [parent] :as oref}]
  (when-not parent
    (throw (ex-info "object has no parent, thus has no siblings" {:oref oref})))

  (get-children parent))

(defn get-siblings-of-type 
  "returns set of all siblings of a common type"
  ([oref]
     (get-siblings-of-type oref oref))
  ([oref type]
     (let [type-kw (if (keyword? type) type (-type type))]
       (filter #(= type-kw (-type %)) (get-siblings oref))
       )))

(defn get-collection-item [oref]
  (let [item-key (::coll-item-key oref)]
    (get oref item-key)
    ))

(defn find-containing-object
  "find the object that contains this dom node"
  [dom]
  (if-let [obj (get-from-dom dom)]
    obj
    (when-let [parent (.-parentElement dom)]
      (recur parent))))

(defn notify! [oref ev & args]
  
  #_ (when-not (contains? @events ev)
       (debug "triggering undefined notifiction" (pr-str ev) " with " (pr-str args)))

  ;; (debug "notify!" oref ev args)

  (when-let [reactions-to-trigger (get-in oref [::reactions ev])]
    ;; (debug "notify!" (-id oref) (-type oref) ev reactions-to-trigger args)
    (doseq [rfn reactions-to-trigger]
      (apply rfn (cons oref args)))
    ))

(defn- do-notify-tree [current-obj ev notify-fn]
  (doseq [child (get-children current-obj)]
    (do-notify-tree child ev notify-fn))
  (notify-fn current-obj))

(defn notify-tree! [oref ev & args]
  (let [notify-fn (fn [obj]
                    (apply notify! obj ev args))]
    (do-notify-tree oref ev notify-fn)))

(def notify-down! notify-tree!)

(defn notify-up! [oref ev & args]
  (loop [current (get-parent oref)]
    (when current
      (apply notify! current ev args)
      (recur (get-parent current)))))

(defn update! [oref update-fn & args]
  (when-not (fn? update-fn)
    (throw (str "update! expects a fn as second arg, not " (pr-str update-fn))))

  (let [id (-id oref)
        data (-data oref)
        work-fn (fn [data] (apply update-fn data args))]
    (-update oref work-fn)
    ))

(defn return-value [oref return-value]
  (update! oref assoc ::return-value return-value))

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
    (if-let [custom-remove (get-type-attr this :dom/remove)]
      (custom-remove this (::dom this))
      (dom/remove (::dom this))
      )))

(defn destroy!
  ([oref]
     (destroy! oref :direct))
  ([oref cause]
     (-destroy! oref cause)))

(defn bind-dom-events [oref dom dom-events]
  (when-not (zero? (rem (count dom-events) 2))
    (throw (ex-info "object defined invalid event" {:object-type (-type oref) :dom/events dom-events})))

  (doseq [[ev handler :as ev-def] (partition 2 dom-events)]
    (when (nil? handler)
      (throw (str "ev with nil handler " (pr-str ev))))

    (let [handler (if (keyword? handler)
                    (fn [this e el]
                      (notify! this handler e el))
                    handler)]
      (dom/on dom ev (fn dom-event-handler [e el]
                       (when (= "A" (.-tagName el)) 
                         ;; FIXME: thou shall not stop events at all?
                         ;; FIXME: stops all events on A, but what except click would you use anyways?
                         ;; <a class="wtf" href="#">something</a>
                         ;; this just simplifies :dom/events [[:click "a.wtf"] some-fn] so some-fn doesnt have to
                         ;; stop the event which is what you'd want 99% of the time, if you dont want to stop it
                         ;; use the router and dont listen to click.
                         (dom/ev-stop e))
                       (handler oref e el))))))

(defn- reaction-merge [result [event handler]]
  (when-not (and event handler)
    (throw (ex-info "invalid reaction" {:event event :handler handler})))

  (let [current (get result event (list))]
    (assoc result event (conj current handler))))

(defn- merge-reactions [result behavior]
  (when-not (sequential? behavior)
    (throw (ex-info "behaviors must be vectors" {:behavior behavior})))
  (when-not (even? (count behavior))
    (throw (ex-info "invalid behavior" {:behavior behavior})))

  (reduce reaction-merge result (reverse (partition 2 behavior))))


(defn- merge-behaviors [result behavior]
  (cond
   (sequential? behavior)
   (update-in result [::reactions] merge-reactions behavior)

   (map? behavior)
   (-> result
       ;; FIXME: there might be more keys to merge, need some kind of merge logic definition
       (update-in [::reactions] merge-reactions (:on behavior []))
       (update-in [:watch] (fn [watches]
                             (concat watches (:watch behavior []))))
       (update-in [:dom/events] (fn [default]
                                  (-> default
                                      (concat (:dom/events behavior []))
                                      (vec)))))
   :else
   (throw (ex-info "behavior not understood" {:behavior behavior}))
   ))

(defn define [id & args]
  (when-not (keyword? id)
    (throw (ex-info "object id needs to be a keyword" {:id id :args args})))
  (when-not (even? (count args))
    (throw (str "invalid object definition " (str id) " args: "(pr-str args))))
  (when (contains? @object-defs id)
    (js/console.warn (str "object already defined " id)))

  (try
    (let [odef (apply hash-map args)

          reactions (merge-reactions {} (:on odef []))

          ;;      reactions (reduce merge-reactions reactions (reverse (:behaviors odef [])))

          odef (assoc odef
                 ::id id
                 ::reactions reactions)
          odef (reduce merge-behaviors odef (reverse (:behaviors odef [])))]

      (swap! object-defs assoc id odef)
      odef)
    (catch js/Object e
      (throw (ex-info "failed to define object" {:id id :args args})))))


(defn- merge-defaults [data type]
  (let [defaults (get-in @object-defs [type :defaults])]
    (cond
     (nil? defaults) data
     (map? defaults) (merge defaults data)
     (fn? defaults) (merge (defaults) data)
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


(defn alive? [obj]
  (contains? @instances (-id obj)))

(defrecord Watch [key handler])

(deftype ObjectRef [id type ^:mutable data ^:mutable watches result-chan]
  IObject
  (-id [this] id)
  (-type [this] type)
  (-data [this] data)
  (-update [this update-fn]
    (let [old data
          new (update-fn data)]
      (set! data new)
      (doseq [{:keys [key handler] :as watch} watches]
        ;; watches may destroy the current object
        ;; if that happens we should not continue
        ;; FIXME: this seems really dirty, there must be a cleaner way?
        ;; maybe queue all other modifications until this is finished?
        (when (alive? this)
          (handler key this old new)))))
  (-destroy! [this cause]
    (let [parent-id (get @instance-parent id)]

      ;; destroy children before destroying parent
      (doseq [child (get-children this)]
        (-destroy! child :parent))

      (notify! this :destroy cause)
      (dom-destroy this cause)
      
      (let [return-value (::return-value this)]
        (when-not (nil? return-value)
          (async/put! result-chan return-value))
        (async/close! result-chan))

      (swap! instances dissoc id)
      (swap! instance-parent dissoc id)
      (when parent-id
        (swap! instance-children update-in [parent-id] disj id))))
  
  async-impl/ReadPort
  (take! [this ^not-native handler]
    (async-impl/take! result-chan handler))

  IEquiv
  (-equiv [this other]
    (and (instance? ObjectRef other)
         (= (-id this) (-id other))))

  IDeref
  (-deref [this] data)

  IPrintWithWriter
  (-pr-writer [this writer opts]
    (-write writer (str "#<ObjectRef {:id " id ", :type " type "}>")))

  IWatchable
  (-notify-watches [this oldval newval]
    (throw (js/Error. "who be calling?")))
  (-add-watch [this key f]
    (set! watches (conj watches (Watch. key f))))
  (-remove-watch [this key]
    (set! watches (remove #(= key (:key %)) watches)))

  ILookup
  (-lookup [this k]
    (if (= :parent k)
      (get-parent this)
      (get data k)))
  (-lookup [this k d]
    (if (= :parent k)
      (get-parent this)
      (get data k d)))

  Object
  (toString [this]
    (pr-str this))

  dom/IElement
  (-to-dom [this] (::dom data))
  
  dom/SVGElement
  (-to-svg [this] (::dom data)))

(defn add-reaction!
  ([oref ev handler-fn]
     (add-reaction! oref [ev handler-fn]))
  ([oref list]
     (update! oref update-in [::reactions] merge-reactions list)
     ))

(defn bind-change
  ([oref attr callback]
     (bind-change oref attr callback (gensym "bind-change")))
  ([oref attr callback watch-key]
     (when-not (satisfies? IObject oref)
       (throw (ex-info "binding currently only supports shadow objects, other atoms might leak, may add later" {:oref oref :attr attr})))

     (let [attr (if (sequential? attr) attr [attr])]
       (add-watch oref watch-key
                  (fn bind-change-watch [_ _ old new]
                    (let [ov (get-in old attr)
                          nv (get-in new attr)]
                      (when-not (= ov nv)
                        (callback ov nv))))))
     ))

(defn dom-enter [parent child]
  (dom/append parent child)
  (when (dom/contains? parent)
    ;; only notify when the parent is already in the dom
    ;; not sure if its useful to keep track of this inside the object itself?
    (notify-tree! child :dom/entered)))

(defn create [type args & node-children]
  (when-not (contains? @object-defs type)
    (throw (ex-info (str "cannot create unknown child type: " type) {:type type :args args})))
  (when-not (map? args)
    (throw (ex-info "so/create second arg must be a map" {:args args})))

  (let [oid (next-id)
        parent (:parent args)
        
        result-chan (async/chan 1)

        odef (get @object-defs type)

        obj (-> args
                (assoc ::object-id oid
                       ::reactions (get odef ::reactions {}))
                (merge-defaults type)
                (dissoc :parent :dom))

        oref (ObjectRef. oid type obj [] result-chan)]

    ;; dont use oref before this
    (swap! instances assoc oid oref)

    (when parent
      (set-parent! oref parent))

    (notify! oref :init)

    (let [dom-events (:dom/events odef [])]
      (if-let [dom (:dom args)]
        ;; attach+events
        (do
          (dom/set-data dom :oid oid)
          (bind-dom-events oref dom dom-events)
          (update! oref assoc ::dom dom)
          (notify! oref :dom/init dom))
        ;; create+events
        (when-let [dom-fn (:dom odef)]
          (let [dom (dom/build (dom-fn oref node-children))]

            (dom/set-data dom :oid oid)

            (update! oref assoc ::dom dom)
            (bind-dom-events oref dom dom-events)
            (notify! oref :dom/init dom)
            ))))

    (when-let [watches (:watch odef)]
      (doseq [[attr handler] (partition 2 watches)]
        (bind-change oref attr (fn [old new]
                                 (handler oref old new)))))

    oref))

(defn bind-simple
  "[oref attr node-gen] produces a node via (node-gen new-value)
   watches obj for changes and replaces the generated node on change (node-gen defaults to str)

  only use if the node has no attached behavior like clicks, use bind with an extra object for those"
  ([oref attr] (bind-simple oref attr str))
  ([oref attr node-gen]
     (let [attr (if (sequential? attr) attr [attr])
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

(defn bind
  "bind the given attribute a child item
  the item will be recreated whenever the value changes (old one will be destroyed)"
  ([oref attr item-type item-key item-attrs]
     (let [attr (if (sequential? attr) attr [attr])
           curval (get-in oref attr)

           make-child-fn (fn [value]
                           (create item-type (merge
                                              item-attrs
                                              {:parent oref
                                               item-key value})))

           child (atom (make-child-fn curval))]

       (bind-change oref attr
                    (fn [old new]
                      (let [new-child (make-child-fn new)
                            current-node @child]

                        (dom/replace-node current-node new-child)
                        (destroy! @child)
                        (reset! child new-child)
                        (notify-down! new-child :dom/entered)
                        )))

       @child)
     ))

(defn coll-destroy-children [children c diff]
  ;; whats more efficient in the DOM, drop head or tail?
  ;; diff is neg!
  (doseq [obj (subvec children (+ c diff) c)]
    (let [obj (get-from-dom obj)]
      (destroy! obj)))

  (subvec children 0 (+ c diff)))

(defn bind-children
  ([node parent attr item-type item-key]
     (bind-children node parent attr item-type item-key #(map-indexed vector %)))
  ([node parent attr item-type item-key coll-transform]
     (let [attr (if (sequential? attr) attr [attr])

           update-children (atom true)

           coll-dom (dom/build node)

           make-item-fn (fn [[key val]]
                          (let [obj (create item-type {:parent parent
                                                       ::coll-path attr
                                                       ::coll-key key
                                                       ::coll-item-key item-key
                                                       item-key val})]

                            (bind-change obj item-key
                                         (fn [old new]
                                           (let [parent-key (conj attr (::coll-key obj))]
                                             (log "direct child update" parent obj key parent-key new)
                                             (reset! update-children false)
                                             (update! parent assoc-in parent-key new)
                                             (reset! update-children true)
                                             )))
                            obj
                            ))

           ]

       (doseq [item (coll-transform (get-in parent attr))]
         (dom-enter coll-dom (make-item-fn item)))

       (bind-change parent attr
                    (fn bind-children-watch [old new]
                      ;; when a child updated itself, we dont need to update it again
                      (when @update-children
                        (let [children (into [] (dom/children coll-dom))
                              new-coll (vec (coll-transform new))
                              count-children (count children)
                              count-new (count new)
                              diff (- count-new count-children)

                              ;; exit lost children
                              children (if (neg? diff)
                                         (coll-destroy-children children count-children diff)
                                         children)
                              count-children (min count-new count-children)]

                          ;; update current
                          (dotimes [idx count-children]
                            (let [cn (nth children idx)
                                  cc (get-from-dom cn)
                                  ckey (::coll-key cc)
                                  cval (get cc item-key)
                                  [nkey nval] (nth new-coll idx)]

                              ;; only update when something changes
                              (when-not (and (= ckey nkey) (= cval nval))
                                ;; lets try this, updating gets way too messy when dealing with complex objects
                                ;; just create a new object and destroy the old, also solves cyclic updates and should be faster
                                (let [new-obj (make-item-fn [nkey nval])]
                                  (dom/replace-node cn new-obj)
                                  (destroy! cc)
                                  (notify-tree! new-obj :dom/entered))
                                ;;(update! cc assoc item-key nval ::coll-key nkey)
                                ;;(notify! cc :bind-child-update ckey nkey cval nval)
                                )))

                          ;; enter new
                          (when (pos? diff)
                            (doseq [item (subvec new-coll count-children count-new)]
                              (dom-enter coll-dom (make-item-fn item))))

                          (notify! parent :bind-children-update)
                          ))))

       coll-dom)))

(defn remove-in-parent! [oref]
  (let [parent (get-parent oref)
        key (::coll-key oref)
        value (get oref (::coll-item-key oref))
        path (::coll-path oref)]

    (when-not (and key path)
      (throw (ex-info "remove-in-parent! should only be called from items created via so/bind-children" {:oref oref})))

    (let [coll (get-in parent path)
          new-coll (util/remove-item-from-coll coll key value)]
      (notify! parent :bind/update path new-coll))
    ))

(defn inspect! [oref]
  (info "inspect!" (-id oref) (str (-type oref)) (clj->js @(-data oref))))

(defn ^:export dump! []
  (info "--------------- LIVE OBJECTS ------------")
  (doseq [[id oref] (seq @instances)]
    (info "dump" id (pr-str (-type oref)) @(-data oref)))
  (info "--------------- //LIVE OBJECTS ------------")
  )
