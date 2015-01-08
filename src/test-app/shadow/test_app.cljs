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
            ;; [item :name] -- sugar?
            (<$ (sc/slice item :name))
            " x: "
            ;; [item :x] -- sugar?
            (<$ (sc/slice item :x))
            )))

(defn replace-random-item [data]
  (let [now (.getTime (js/Date.))]
    (sc/update! data update-in [:coll] (fn [coll]
                                         (assoc coll
                                           (int (rand (count coll)))
                                           {:id now
                                            :name (str "item" now)})))))

(defn remove-random-item [data]
  (let [now (.getTime (js/Date.))]
    (sc/update! data update-in [:coll] (fn [coll]
                                         (let [idx (int (rand (count coll)))]
                                           (util/remove-from-vector coll idx))
                                         ))))

(defn add-item [data]
  (let [now (.getTime (js/Date.))]
    (sc/update! data update-in [:coll] conj {:id now
                                             :name (str "item" now)})))

(defn remove-first-item [data]
  (let [now (.getTime (js/Date.))]
    (sc/update! data update-in [:coll] (fn [coll] (into [] (rest coll))))))

(defn remove-last-item [data]
  (let [now (.getTime (js/Date.))]
    (sc/update! data update-in [:coll] (fn [coll] (into [] (butlast coll))))))

(defc test-component
  :init (fn [this])

  :dom/init (fn [this el])
  
  :dom (fn [{:keys [data] :as this} _]
         (let [object-c (sc/slice data :object)]
           ($ html/div
              ($ html/h1
                 "Hello "
                 (<$ (sc/slice data [:name])
                     (fn [cursor]
                       (if (seq @cursor)
                         @cursor
                         "Stranger")))
                 "!")
              
              ($ (html/button
                  (sc/on :click #(add-item data)))
                 "add item")

              ($ (html/button
                  (sc/on :click #(dotimes [i 100]
                                   (add-item data))))
                 "add 100 items")

              ($ (html/button
                  (sc/on :click #(replace-random-item data)))
                 "replace random item")

              ($ (html/button
                  (sc/on :click #(remove-random-item data)))
                 "remove random item")

              ($ (html/button
                  (sc/on :click #(remove-first-item data)))
                 "remove first item")

              ($ (html/button
                  (sc/on :click #(remove-last-item data)))
                 "remove last item")

              ($ html/ul
                 
                 ($ html/li "this is not managed")

                 (<$* (sc/slice data :coll)
                      {:key :id
                       :dom (fn [cursor]
                              (coll-item-view {:item cursor}))})

                 ($ html/li "this is not managed"))
              

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
                      :coll (into [] (for [i (range 10)]
                                       {:id i
                                        :name (str "item" i)
                                        :x 0}))}))

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
