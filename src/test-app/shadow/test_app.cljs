(ns shadow.test-app
  (:require-macros [cljs.core.async.macros :refer (go alt!)])
  (:require [cljs.core.async :as async]
            [shadow.components :as sc :refer ($ <$ <$* defc)]
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
  [cursor]
  (let [ {:keys [id name]} @cursor]
    (str "object #" id " - " name)))

(def div-title
  (html/div {:class "title"}))

(def div-form-group
  (html/div {:class "form-group"}))

(def btn-default
  (html/button
   {:type "button" :class "btn btn-default"}))

(defc object-display
  :dom (fn [{:keys [object-c] :as this} _]
         ($ html/div
            ($ html/h2 "OBJECT!")
            ($ div-title (<$ object-c object-title))
            (<$ (sc/slice object-c [:i]))
            )))

(defn inc-clicks [data e el]
  (sc/update! data update :clicks inc))

(defn clicks-text [clicks]
  (let [val @clicks]
    (cond
     (zero? val)
     "No clicks? :("

     (= val 1)
     "Only once?"

     (< val 25)
     (str val " Clicks!")
     
     :else
     (str val " Clicks! Sir Click-A-Lot!")
     )))


(defc coll-item-view
  :dom (fn [{:keys [item] :as this}]
         ($ html/li
            ($ (html/button
                (sc/on :click (fn [e el]
                                (dom/ev-stop e)
                                (sc/update! item update-in [:x] inc))))
               "click me")
            " "
            ;; [item :name] -- sugar?
            (<$ (sc/slice item :name))
            " x: "
            ;; [item :x] -- sugar?
            (<$ (sc/slice item :x))
            )))

(def next-id
  (let [id-seq (atom 0)]
    (fn []
      (str "$$next-id" (swap! id-seq inc)))))

(defn replace-random-item [data]
  (let [id (next-id)]
    (sc/update! data update-in [:coll] (fn [coll]
                                         (assoc coll
                                           (int (rand (count coll)))
                                           {:id id
                                            :name (str "item" id)
                                            :x 0})))))

(defn remove-random-item [data]
  (sc/update! data update-in [:coll] (fn [coll]
                                       (let [idx (int (rand (count coll)))]
                                         (util/remove-from-vector coll idx))
                                       )))

(defn add-item [data]
  (let [id (next-id)]
    (sc/update! data update-in [:coll] conj {:id id
                                             :name (str "item" id)
                                             :x 0})))

(defn remove-first-item [data]
  (let [now (.getTime (js/Date.))]
    (sc/update! data update-in [:coll] (fn [coll] (into [] (rest coll))))))

(defn remove-last-item [data]
  (let [now (.getTime (js/Date.))]
    (sc/update! data update-in [:coll] (fn [coll] (into [] (butlast coll))))))

(defn remove-all [data]
  (sc/update! data assoc :coll []))

(defn switch-tabs [{:keys [selected] :as this} id idx]
  (sc/update! selected (fn [_] id))
  (sc/update! this assoc :selected-index idx))

(defc tabs-c
  :init (fn [{:keys [selected] :as this}]
          (when (nil? @selected)
            (sc/update! this assoc :selected-index 0)
            (sc/update! selected (fn [_] (-> this :tabs first :id)))))

  :dom (fn [{:keys [selected tabs] :as this} _]
         (let [bar-width (/ 100 (count tabs))]
           ($ (html/div {:class "sm-tabs"})
              
              ($ (html/div
                  {:class "selection-bar"
                   :style (str "width: " bar-width "%;")}
                  (sc/bind (sc/slice this :selected-index)
                           (fn [el idx]
                             (dom/set-style el {:left (dom/pct (* idx bar-width))})
                             ))))

              (for [[idx {:keys [id title]}] (map-indexed vector tabs)]
                ($ (html/div
                    {:class "sm-tab"}
                    (sc/on :click #(switch-tabs this id idx))
                    (sc/bind selected (fn [el selected]
                                        (dom/toggle-class el "tab-selected" (= id selected)))))
                   title))

              ))))

(defn tabs [cursor tabs]
  (tabs-c {:selected cursor
           :tabs tabs}))

(defc test-component
  :init (fn [this])

  :dom/init (fn [this el])
  
  :dom (fn [{:keys [data] :as this} _]
         (let [object-c (sc/slice data :object)]
           ($ html/div
              
              (tabs (sc/slice data :current-tab)
                    [{:id :item-one
                      :title "Item One"}
                     {:id :item-two
                      :title "Item Two"}
                     {:id :item-three
                      :title "Item Three"}])
              
              ($ html/h1
                 "Hello "
                 (<$ (sc/slice data [:name])
                     (fn [cursor]
                       (if (seq @cursor)
                         @cursor
                         "Stranger")))
                 "!")
              
              (for [[title action] [["add item" #(add-item data)]
                                    ["add 200 items" #(dotimes [i 200] (add-item data))]
                                    ["replace random item" #(replace-random-item data)]
                                    ["remove random item" #(remove-random-item data)]
                                    ["remove first item" #(remove-first-item data)]
                                    ["remove last item" #(remove-last-item data)]
                                    ["remove all" #(remove-all data)]]]
                ($ (html/button
                    (sc/on :click action))
                   title))

              
              
              ($ html/ul
                 
                 ($ html/li "before (unmanaged)")

                 (<$* (sc/slice data :coll)
                      {:key :id
                       :dom (fn [cursor]
                              (coll-item-view {:item cursor}))})

                 ($ html/li "after (unmanaged)"))
              

              ($ (html/form
                  (sc/on :submit dom/ev-stop))

                 ($ div-form-group
                    ($ html/label "What is your name?")

                    (html/input
                     {:class "form-control" :placeholder "..." :autofocus true}
                     (sc/on :keyup (fn [e el]
                                     (sc/update! data assoc :name (dom/get-value el))))))
                 ($ div-form-group
                    ($ (btn-default
                        (ripple/for-element this)
                        (sc/on :click (partial inc-clicks data)))
                       "Click me, I do Stuff!"))

                 ($ div-form-group
                    (<$ (sc/slice data [:clicks]) clicks-text)))
              

              ($ html/div

                 ($ (btn-default
                     (sc/on :click (fn [e el]
                                     (toast/display this {} "Hello World!"))))
                    "toast") 

                 ($ (btn-default
                     (sc/on :click #(sc/update! data assoc :object {:id 1
                                                                    :name "obj1"
                                                                    :i 0})))
                    "swap obj 1") 
                 
                 
                 ($ (btn-default
                     (sc/on :click #(sc/update! data assoc :object {:id 2
                                                                    :name "obj2"
                                                                    :i 0})))
                    "swap obj 2")

                 ($ (btn-default
                     (sc/on :click (fn [e el]
                                     (let [ref (get-in this [:refs :display])]
                                       (when ref
                                         (sc/update! data update-in [:object :i] inc))))))
                    "inc i")

                 ($ (btn-default
                     (sc/on :click #(sc/update! data dissoc :object)))
                    "remove obj")

                 ($ (btn-default
                     (sc/on :click dt/scope-snapshot))
                    "scope snapshot"))
              
              (<$ object-c
                  {:key :id
                   :dom (fn [cursor]
                          (when @cursor
                            ($ (object-display {:object-c cursor}
                                               (sc/set-ref this [:refs :display])
                                               (fn [el scope]
                                                 (go (<! el)
                                                     (log "object display died")))
                                               ))
                            ))})

              
              ))))

(def test-data (atom {:name ""
                      :clicks 0
                      :coll []}))

(add-watch test-data :dump (fn [_ _ _ new]
                             (log "test-data" new)))

(def root-c (atom nil))

(defn ^:export start [ref]
  (log "START")

  (let [root (sc/construct (test-component {:data (sc/root-cursor test-data)}))]
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
