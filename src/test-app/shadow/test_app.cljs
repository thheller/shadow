(ns shadow.test-app
  (:require-macros [cljs.core.async.macros :refer (go alt!)])
  (:require [cljs.core.async :as async]
            [shadow.components :as sc :refer ($ <$ defc)]
            [shadow.util :as util :refer (log with-timing)]
            [shadow.api :as api :refer (ns-ready)]
            [shadow.html :as html]
            [shadow.dom :as dom]
            [shadow.animate :as anim]
            [shadow.material.toast :as toast]
            [shadow.material.ripple :as ripple]
            [shadow.dev-tools :as dt]
            ))




(defn object-title
  [{:keys [id name] :as object}]
  (str "object #" id " - " name))

(defprotocol ISlice
  (-slice [this path]))

(deftype Cursor [root path]
  sc/IPull
  (-pull! [this]
    (get-in (sc/-pull! root) path))

  IDeref
  (-deref [_]
    (get-in @root path))

  IWatchable
  (-add-watch [this key callback]
    (-add-watch root key callback))
  (-remove-watch [this key]
    (-remove-watch root key))

  IEquiv
  (-equiv [this other]
    (and (instance? Cursor other)
         (-equiv root (.-root other))
         (-equiv path (.-path other))))

  ISlice
  (-slice [this new-path]
    (Cursor. root (into path new-path)))

  IHash
  (-hash [this]
    (js/goog.getUid this))
  
  IPrintWithWriter
  (-pr-writer [_ w opts]
    (-write w "#<Cursor ")
    (-pr-writer path w opts)
    (-write w " ")
    (-pr-writer root w opts)
    (-write w ">"))
  
  Object
  (toString [this]
    (str "#<Cursor " path " " root ">")))

(extend-protocol ISlice
  Atom
  (-slice [this path]
    (Cursor. this path)))

(defn cursor [src path]
  (if (sequential? path)
    (-slice src path)
    (-slice src [path])))

(def div-title
  (html/div {:class "title"}))

(def div-form-group
  (html/div {:class "form-group"}))

(def btn-default
  (html/button
   {:type "button" :class "btn btn-default"}))


(defc object-display
  :triggers [:object-c]

  :dom (fn [{:keys [object-c] :as this} _]
         ($ html/div
            ($ html/h2 "OBJECT!")
            ($ div-title (<$ object-c object-title))
            (<$ [object-c :i] str))))

(defn inc-clicks [data e el]
  (swap! data update :clicks inc))

(defn clicks-text [clicks]
  (cond
   (zero? clicks)
   "No clicks? :("

   (= clicks 1)
   "Only once?"

   (< clicks 25)
   (str clicks " Clicks!")
   
   :else
   (str clicks " Clicks! Sir Click-A-Lot!")
   ))

(defc test-component
  :triggers [:data]
  
  :init (fn [this])

  :dom/init (fn [this el])
  
  :dom (fn [{:keys [data] :as this} _]
         (let [object-c (cursor data :object)]
           ($ html/div
              ($ html/h1
                 "Hello "
                 (<$ [data :name] (fn [value]
                                    (if (seq value)
                                      value
                                      "Stranger")))
                 "!")
              
              ($ (html/form
                  (sc/on :submit (fn [e el]
                                   (dom/ev-stop e)
                                   (log "cancelled submit" e el))))

                 ($ div-form-group
                    ($ html/label "What is your name?")

                    (html/input
                     {:class "form-control" :placeholder "..." :autofocus true}
                     (sc/on :keyup (fn [e el]
                                     (swap! data assoc :name (dom/get-value el))))))
                 ($ div-form-group
                    ($ (btn-default
                        (ripple/for-element this)
                        (sc/on :click (partial inc-clicks data)))
                       "Click me, I do Stuff!"))

                 ($ div-form-group
                    (<$ [data :clicks] clicks-text)))

              
              
              ($ html/div

                 ($ (btn-default
                     (ripple/for-element this)
                     (sc/on :click (fn [e el]
                                     (toast/display this {} "Hello World!"))))
                    "toast") 

                 ($ (btn-default
                     (ripple/for-element this)
                     (sc/on :click #(swap! data assoc :object {:id 1
                                                               :name "obj1"
                                                               :i 0})))
                    "swap obj 1") 
                 
                 
                 ($ (btn-default
                     (ripple/for-element this)
                     (sc/on :click #(swap! data assoc :object {:id 2
                                                               :name "obj2"
                                                               :i 0})))
                    "swap obj 2")

                 ($ (btn-default
                     (ripple/for-element this)
                     (sc/on :click (fn [e el]
                                     (let [ref (get-in this [:refs :display])]
                                       (when ref
                                         (swap! data update-in [:object :i] inc))))))
                    "inc i")

                 ($ (btn-default
                     (ripple/for-element this)
                     (sc/on :click #(swap! data dissoc :object)))
                    "remove obj")

                 ($ (btn-default
                     (ripple/for-element this)
                     (sc/on :click dt/scope-snapshot))
                    "scope snapshot"))

              (<$ object-c
                  {:key :id
                   :dom (fn [data]
                          (when data
                            ($ (object-display {:object-c object-c}
                                               (sc/set-ref this [:refs :display])
                                               (fn [el scope]
                                                 (go (<! el)
                                                     (log "object display died")))
                                               ))
                            ))})

              
              ))))

(def test-data (atom {:name ""
                      :clicks 0}))

(def root-c (atom nil))

(defn ^:export start [ref]
  (log "START")
  
  (let [root (sc/construct (test-component {:data test-data}))]
    (reset! root-c root)
    (dom/insert-before ref root)
    
    ;; (toast/display root {} ($ html/h2 "Welcome!"))
    ))

(defn ^:export stop []
  (log "STOP")

  (when-let [r @root-c]
    (sc/destroy! r)
    (reset! root-c nil)))

(ns-ready)
