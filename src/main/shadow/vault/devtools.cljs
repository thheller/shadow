(ns shadow.vault.devtools
  (:require [shadow.vault.store :as store :refer (defaction defkey)]
            [shadow.vault.dom :as vdom]
            [shadow.react.component :as comp :refer (deffactory)]
            [shadow.markup.react :as html]
            [shadow.markup.css :as css :refer (defstyled)]
            [shadow.dom :as dom]
            [fipp.edn :refer (pprint)]
            [cljs.spec.alpha :as s]))

;; (declare render-object)
(defn render-object [obj])

(defstyled container :div
  [_]
  {:position "fixed"
   :left 0
   :top 0
   :bottom 0
   :right 0
   :overflow "auto"
   :background "white"
   :border "1px solid red"
   :padding 10
   :z-index 1000
   :margin 0})

(defstyled browser :div
  [_]
  {:display "flex"})

(defstyled ns-listing :div
  [_]
  {:width 300})

(defstyled key-listing :div
  [_]
  {:flex 1})

(defstyled fab :div
  [_]
  {:position "fixed"
   :bottom 10
   :left 10
   :background-color "red"
   :color "#fff"
   :font-weight "bold"
   :border-radius 20
   :padding 20})


(defaction init)

(defaction toggle)

(defaction set-namespace
  string?)

(defaction set-key
  store/key?)

(defkey Namespaces
  :spec
  (s/coll-of string?))

(defkey Keys
  :spec
  (s/coll-of store/key?))

(defkey Settings
  :spec
  any?
  :init
  (fn [_]
    {:show? false
     :namespace nil
     :key nil}))

(defn inspector-handler
  [vault action inspector]
  (js/console.log "inspector vault" vault (store/keys vault))
  (store/action-case action
    [init _]
    (-> vault
        (assoc Namespaces
          (->> (store/keys vault)
               (map :tag)
               (map namespace)
               (distinct)
               (sort)
               (into []))))

    [set-namespace ns]
    (-> vault
        (update Settings assoc
          :namespace ns
          :key nil)
        (assoc Keys
          (->> (store/keys vault)
               (filter #(= ns (-> % :tag namespace)))
               (sort-by #(-> % :tag name))
               (into []))))

    [set-key key]
    (-> vault
        (update Settings assoc :key key))

    [toggle _]
    (-> vault
        (update-in [Settings :show?] not))

    vault))

(def Inspector
  )

(deffactory inspector
  ::comp/mixins
  [store/mixin]

  ::comp/type
  ::inspector

  ::store/handlers
  [inspector-handler]


  ::comp/will-mount
  (fn [{::store/keys [vault] :as this}]
    (store/transact! vault [(init)])
    this)

  ::store/read
  (fn [this vault props]
    (let [{:keys [key] :as settings}
          (get vault Settings)]
      {:settings settings
       :namespaces (get vault Namespaces)
       :keys (get vault Keys)
       :key-value (when key (get vault key))}))

  ::store/render
  (fn [this vault props
       {:keys
        [settings
         namespaces
         keys
         key-value]
        :as data}]
    (if-not (:show? settings)
      (fab {:onClick
            (fn [e]
              (store/transact! vault [(toggle)]))}
        "+")

      (container {}
        (html/div
          {:onClick
           (fn [e]
             (store/transact! vault [(toggle)]))}
          (pr-str settings))

        (browser {}
          (ns-listing
            (html/for [ns namespaces]
              (html/div
                {:onClick
                 (fn [e]
                   (store/transact! vault [(set-namespace ns)]))}
                ns)))

          (key-listing
            (html/for [key keys]
              (html/div
                {:onClick
                 (fn [e]
                   (store/transact! vault [(set-key key)]))}
                (let [{:keys [tag id]} key]
                  (pr-str [(name tag) id])))))

          )
        (when key-value
          (render-object key-value))
        ))))

(deftype DevVault [delegate]
  store/IBranch
  (branch [this handlers]
    (DevVault. (store/branch delegate handlers)))

  IDeref
  (-deref [this]
    @delegate)

  store/ITransact
  (transact! [this actions]
    (let [result (store/transact! delegate actions)]
      (js/console.log "devtools/transact" result)
      result
      )))

;; DBG

(defn pp [x]
  (with-out-str
    (pprint x)))

(defstyled html-keyword :span
  [_]
  {:white-space "nowrap"
   :background-color "#f3f1ff"
   :color "#7f54ca"
   })

(defstyled html-symbol :span
  [_]
  {})

(defstyled html-string :span
  [_]
  {})

(defstyled html-lookup :table
  [_]
  {:width "100%"})

(defstyled html-lookup-row :tr
  [_]
  {
   "& > td"
   {:border "1px solid #ddd"
    :padding 2}})

(defstyled html-lookup-key :td
  [_]
  {:width 1
   :vertical-align "top"})

(defstyled html-lookup-value :td
  [_]
  {})

(defstyled html-seq :table
  [_]
  {:width "100%"})

(defstyled html-seq-row :tr
  [_]
  {
   "& > td"
   {:border "1px solid #ddd"
    :padding 2}})

(defstyled html-seq-idx :td
  [_]
  {:width 1
   :color "#aaa"})

(defstyled html-seq-value :td
  [_]
  {})

(defstyled html-caption :caption
  [_]
  {:text-align "left"
   :padding 4
   :background-color "#ddd"})

(defn render-seq [obj]
  (html-seq {}
    (html-caption {} (str (pr-str (type obj)) " [count: " (count obj) "]"))
    (html/tbody {}
      (html/for [[idx value] (map-indexed vector obj)]
        (html-seq-row {}
          (html-seq-idx {} idx)
          (html-seq-value {} (render-object value)))
        ))))

(defn render-ilookup [obj]
  (html-lookup {}
    (html-caption {} (str (pr-str (type obj)) " [count: " (count obj) "]"))
    (html/tbody {}
      (let [keys
            (keys obj)

            keys
            (try
              (sort keys)
              (catch :default e
                keys))]
        (html/for [key keys
                   :let [value (get obj key)]]
          (html-lookup-row {}
            (html-lookup-key {} (render-object key))
            (html-lookup-value {} (render-object value))
            ))))))

(defn render-object [obj]
  (cond
    (nil? obj)
    (html-symbol {} "nil")

    (keyword? obj)
    (html-keyword {} (str obj))

    (symbol? obj)
    (html-symbol {} (str obj))

    (boolean? obj)
    (html-symbol {} (str obj))

    (number? obj)
    (html-symbol {} (str obj))

    (string? obj)
    (html-string {} obj)

    (or (implements? cljs.core/ISequential obj)
        (implements? cljs.core/ISet obj))
    (render-seq obj)

    (implements? cljs.core/ILookup obj)
    (render-ilookup obj)

    :else
    (html/pre (pr-str obj))
    ))

(def Dump
  )

(def dump
  ::comp/mixins
  [store/mixin]

  ::comp/type
  ::dump

  ::store/read
  (fn [this vault {:keys [path] :as props}]
    {:obj (get-in vault path)})

  ::comp/render
  (fn [this]
    (render-object (-> this :data :obj))))

(defn dump-into [host context path]
  (vdom/mount host (dump {:path path}) context))

(defn context [ctx]
  (let [{::store/keys [vault]}
        ctx]
    (assoc ctx ::store/vault (DevVault. vault))
    ))


(comment
  (defonce history-ref (volatile! []))

  (defn tx-callback [{:keys [keys-new keys-updated keys-removed] :as tx}]
    (js/console.log
      (str "TX-SUMMARY"
           " new:" (count keys-new)
           " updated:" (count keys-updated)
           " removed:" (count keys-removed))
      tx))

  (store/add-tx-callback! ::devtools tx-callback)

  (defonce devtools-dom (dom/append [:div#devtools-root]))

  (vdom/unmount devtools-dom)
  (vdom/mount devtools-dom (inspector {}) {}))
