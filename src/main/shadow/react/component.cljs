(ns shadow.react.component
  "EXPERIMENTAL - DO NOT USE"
  (:require-macros [shadow.react.component :as m])
  (:require [cljsjs.react]
            [cljs.spec :as s]))

(defonce active-components-ref (volatile! {}))

(defrecord ShadowRef [type id]
  cljs.core/IDeref
  (-deref [this]
    (get @active-components-ref this)))

(def id-seq-ref
  (volatile! 0))

(defn ref? [x]
  (and x (instance? ShadowRef x)))

(defn react? [x]
  (and (some? x)
       (map? (js/goog.object.get x "shadow$component"))))

(defn shadow? [x]
  (and x
       (map? x)
       (ref? (::ref x))))

(defn get-shadow-props [props]
  (js/goog.object.get props "shadow$props"))

(defn get-shadow [react]
  (when-not (react? react)
    (throw (ex-info "invalid react ref" {:ref react})))
  (js/goog.object.get react "shadow$component"))

(defn set-shadow [react data]
  {:pre [(react? react)]}
  (js/goog.object.set react "shadow$component" data))

(defn get-ref [react]
  (-> react (get-shadow) (::ref)))

(defn update-data [component update-fn]
  (let [data (get-shadow component)]
    (let [new-data (update-fn data)]
      (set-shadow component new-data)
      component
      )))

(defn ref-update!
  [ref update-fn]
  {:pre [(ref? ref)]}
  (let [instance
        @ref

        result
        (update-data instance update-fn)]
    ;; FIXME: go through store, don't do this here

    (.forceUpdate instance)
    result
    ))

(defn- get-query-fn [target query-id]
  (let [{::keys [config] :as data}
        (cond
          (react? target)
          (get-shadow target)

          (shadow? target)
          target

          :else
          (throw (ex-info "cannot query target" {:target target :query-id query-id})))]

    [(get config query-id) data]
    ))

(defn- check-call-result! [after before call-id]
  (when-not (identical? (::config before) (::config after))
    (throw (ex-info (str "invalid call result " call-id " on " (-> before ::config ::type))
             {:call-id call-id
              :after after
              :before before})))

  after)

(defn- get-call-fn [data call-id]
  {:pre [(shadow? data)]}
  (let [config (::config data)]
    (get config call-id)))

;; FIXME: lots of manual args inlining to avoid apply
;; probably better strategy to inline the most frequently called
;; functions like ::comp/render so we don't look it up every time
;; just inline them when creating the prototype so we look them up once
;; and not for every call/query

(defn query
  ([target query-id]
   (let [[query-fn data] (get-query-fn target query-id)]
     (when query-fn
       (query-fn data))))
  ([target query-id a1]
   (let [[query-fn data] (get-query-fn target query-id)]
     (when query-fn
       (query-fn data a1))))
  ([target query-id a1 a2]
   (let [[query-fn data] (get-query-fn target query-id)]
     (when query-fn
       (query-fn data a1 a2))))
  ([target query-id a1 a2 & more]
   (let [[query-fn data] (get-query-fn target query-id)]
     (when query-fn
       (apply query-fn data a1 a2 more)))))

(defn call
  ([data call-id]
   (if-let [call-fn (get-call-fn data call-id)]
     (let [result (call-fn data)]
       (check-call-result! result data call-id)
       result)
     data))
  ([data call-id a1]
   (if-let [call-fn (get-call-fn data call-id)]
     (let [result (call-fn data a1)]
       (check-call-result! result data call-id)
       result)
     data))
  ([data call-id a1 a2]
   (if-let [call-fn (get-call-fn data call-id)]
     (let [result (call-fn data a1 a2)]
       (check-call-result! result data call-id)
       result)
     data))
  ([data call-id a1 a2 & more]
   (if-let [call-fn (get-call-fn data call-id)]
     (let [result (apply call-fn data a1 a2 more)]
       (check-call-result! result data call-id)
       result)
     data)))

(defn call-ref
  ([ref call-id]
   {:pre [(ref? ref)]}
   (when-let [component (get @active-components-ref ref)]
     (let [data
           (get-shadow component)

           new-data
           (call data call-id)]

       (set-shadow component new-data)
       new-data
       )))

  ([ref call-id a1]
   {:pre [(ref? ref)]}
   (when-let [component (get @active-components-ref ref)]
     (let [data
           (get-shadow component)

           new-data
           (call data call-id a1)]

       (set-shadow component new-data)
       new-data
       )))

  ([ref call-id a1 a2]
   {:pre [(ref? ref)]}
   (when-let [component (get @active-components-ref ref)]
     (let [data
           (get-shadow component)

           new-data
           (call data call-id a1 a2)]

       (set-shadow component new-data)
       new-data
       )))

  ([ref call-id a1 a2 & more]
   {:pre [(ref? ref)]}
   (when-let [component (get @active-components-ref ref)]
     (let [data
           (get-shadow component)

           new-data
           (apply call data call-id a1 a2 more)]

       (set-shadow component new-data)
       new-data
       ))))

(defn force-update [ref]
  {:pre [(ref? ref)]}
  (when-let [react @ref]
    (.forceUpdate react)))


;; these are on the component constructor fn not on the prototype
(def context-static-props
  #js {:childContextTypes
       #js {:shadow$context
            js/React.PropTypes.object}

       :contextTypes
       #js {:shadow$context
            js/React.PropTypes.object}})

;; FIXME: create fast accessors for functions that are frequently called
;; so (call ::process-props) doesn't do a prop access to get the data
;; then a map lookup to get the fn
;; better if we could just call the fn directly as this is called a lot

(defn make-component-prototype
  [{::keys [should-update? render] :as config}]
  #js {:shadow$config
       config

       :getChildContext
       (fn []
         (this-as this
           (let [{::keys [context] :as data}
                 (get-shadow this)]

             #js {:shadow$context context}
             )))

       :componentWillReceiveProps
       (fn [next-props]
         (this-as this
           (update-data
             this
             (fn [instance]
               (-> instance
                   (assoc :pending-props (get-shadow-props next-props))
                   (call ::will-receive-props))))))

       :componentWillMount
       (fn []
         (this-as this
           (vswap! active-components-ref assoc (get-ref this) this)

           (update-data this
             (fn [{:keys [pending-props] :as data}]
               (-> data
                   (assoc
                     :props pending-props
                     :pending-props nil)
                   (call ::will-mount))))))

       :componentWillUpdate
       (fn [next-props next-state]
         (this-as this
           (update-data this
             (fn [{:keys [pending-props] :as data}]
               (-> data
                   (cond->
                     pending-props
                     (assoc
                       :props pending-props
                       :pending-props nil))
                   (call ::will-update))))
           ))

       :componentDidUpdate
       (fn [prev-props prev-state]
         (this-as this
           (update-data this
             (fn [{:keys [props] :as data}]
               (-> data
                   (assoc :prev-props props)
                   (call ::did-update))))
           ))

       :componentDidMount
       (fn []
         (this-as this
           (update-data this
             (fn [{:keys [props] :as data}]
               (-> data
                   (assoc :prev-props props)
                   (call ::did-mount))))))

       :componentWillUnmount
       (fn []
         (this-as this
           (update-data this #(call % ::will-unmount))
           (vswap! active-components-ref dissoc (get-ref this))))

       :shouldComponentUpdate
       (fn [next-props next-state]
         (this-as this
           (if (nil? should-update?)
             true
             (should-update? (get-shadow this))
             )))

       :render
       (fn []
         (this-as this
           (render (get-shadow this))
           ))})

(defn create-element* [component-fn props children]
  (let [{::keys [type key-fn] :as config}
        (-> component-fn (js/goog.object.get "shadow$config"))]

    (when-some [props-spec (::props config)]
      (when-not (s/spec? props-spec)
        (throw (ex-info "invalid props spec for type" {:type type})))

      (when-not (s/valid? props-spec props)
        ;; FIXME: remove explain, probably nicer to not throw
        ;; maybe just render a dummy error element?
        (s/explain props-spec props)
        (throw (ex-info (str "invalid props for component " type)
                 (assoc (s/explain-data props-spec props)
                   ::type type
                   ::props props)))))

    (let [final-props
          (dissoc props :react-key :react-ref)

          react-props
          #js {:shadow$props final-props}]

      (when-let [ref (:react-ref props)]
        (js/goog.object.set react-props "ref" ref))

      (if key-fn
        (let [key (key-fn props)]
          (when (nil? key)
            (throw (ex-info (str ":key-fn on " type " was set but returned nil") props)))
          (js/goog.object.set react-props "key" key))

        (when-let [key (:react-key props)]
          (js/goog.object.set react-props "key" key)))

      (js/goog.object.set
        react-props
        "children"
        (let [c (bounded-count 2 children)]
          (condp = c
            0 nil
            1 (first children)
            children)))

      (js/React.createElement component-fn react-props))))


(defn make-component
  [{::keys
    [type
     render
     rewrite-props]

    :as
    config}]
  ;; FIXME: spec for config (must have type and render)
  {:pre [(keyword? type)
         (ifn? render)]}

  (let [component-fn
        (fn component-fn [props context updater]
          ;; CLJS/shadow/react/component.js:527: ERROR - incorrect use of goog.base: First argument must be 'this'.
          ;; goog.base(this$,props,context,updater);

          (cljs.core/this-as this
            (js/React.Component.call this props context updater)

            (let [ref
                  (ShadowRef. type (vswap! id-seq-ref inc))

                  shadow-props
                  (-> (get-shadow-props props)
                      (cond->
                        (fn? rewrite-props)
                        (rewrite-props)))

                  data
                  (-> {::config config
                       ::ref ref
                       ::context (js/goog.object.get context "shadow$context")
                       :prev-props nil
                       :props shadow-props
                       :pending-props shadow-props}
                      (call ::constructor this))]

              ;; don't use set-shadow, doesn't survive the react? check
              (js/goog.object.set this "shadow$component" data))

            this))

        display-name
        (subs (str type) 1)]

    (js/goog.object.extend
      (.. component-fn -prototype)
      js/React.Component.prototype
      (make-component-prototype config))

    (set! (.. component-fn -displayName) display-name)

    ;; cljs-specific properties on constructor
    (set! (.-cljs$lang$type component-fn) true)
    (set! (.-cljs$lang$ctorStr component-fn) display-name)
    (set! (.-cljs$lang$ctorPrWriter component-fn)
      (fn [this writer opt]
        (cljs.core/-write writer display-name)))
    (set! (.. component-fn -prototype -constructor) component-fn)

    (js/goog.object.extend component-fn context-static-props)

    (when-let [extend (::extend config)]
      (js/goog.object.extend (.. component-fn -prototype) extend))

    (when-let [static (::static config)]
      (js/goog.object.extend component-fn static))

    (js/goog.object.set component-fn "shadow$config" config)

    component-fn
    ))

(defn factory
  "use deffactory over this since this cannot be removed by Closure if never accessed"
  [config]
  (let [component-fn
        (make-component config)

        factory-fn
        (fn factory-fn [props & children]
          (create-element* component-fn props children))]

    ;; shadow$component must be a function since component is initialized lazily in macro
    (js/goog.object.set factory-fn "shadow$component" #(js/goog.object.get component-fn "shadow$config"))

    factory-fn))

(defn is-shadow-factory? [x]
  (and (fn? x) (fn? (js/goog.object.get x "shadow$component"))))

(defn get-factory-config [x]
  {:pre [(is-shadow-factory? x)]}

  (let [getter
        (js/goog.object.get x "shadow$component")]

    (getter)
    ))

(defn after
  [config id after-fn]
  (let [actual-fn (get config id)]
    (if (nil? actual-fn)
      (assoc config id after-fn)

      (assoc config id
        (fn
          ([this]
           (-> this
               (actual-fn)
               (after-fn)))
          ([this a1]
           (-> this
               (actual-fn a1)
               (after-fn a1)))
          ([this a1 a2]
           (-> this
               (actual-fn a1 a2)
               (after-fn a1 a2)))
          ([this a1 a2 & more]
           (as-> this this
             (apply actual-fn this a1 a2 more)
             (apply after-fn this a1 a2 more)))
          )))))

(defn get-react [x]
  (cond
    (react? x)
    x

    (shadow? x)
    (get @active-components-ref (::ref x))

    (ref? x)
    @x

    :else
    (throw (ex-info "can't get react instance from" {:x x}))))

(defn dom-node
  ([component]
   (-> component
       (get-react)
       (js/ReactDOM.findDOMNode)))
  ([component ref]
   {:pre [(string? ref)]}
   (-> component
       (get-react)
       (.-refs)
       (js/goog.object.get ref))))
