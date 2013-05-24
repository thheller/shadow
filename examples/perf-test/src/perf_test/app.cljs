(ns perf-test.app
  (:require [shadow.object :as so]
            [shadow.dom :as dom]))

(so/define ::item
  :dom (fn [this]
         [:li (so/bind this :value)]))

(set! *print-fn* (fn [s] (when-not (= s "\n")
                           (.log js/console s))))

(def cycles 1000)

(defn run-cycle [app]
  (so/log "going for" cycles)
  (let [prof (str cycles " cycles")]
    (.profile js/console prof)
    (time
     (dotimes [n cycles]
       (so/update! app update-in [:items (rand-int 100)] inc)
       ;; (so/update! app assoc :items (vec (map rand-int (range 100))))
       ))
    (.profileEnd js/console prof)))

(defn run-animation [app]
  (let [frame-fn (fn frame-fn []
                   (so/update! app update-in [:items (rand-int 100)] inc)
                   (.requestAnimationFrame js/window frame-fn)
                   )]
    (.requestAnimationFrame js/window frame-fn)
    ))

(so/define ::app
  :dom (fn [this]
         [:div#app
          [:h1 "Shadow Perf Test"]
          [:button.go "Go!"]
          [:button.anim "Inifinite Animation!"]

          (so/bind-children :ul.items
                            this :items
                            ::item :value)
          ])

  :dom/events [[:click "button.go"] run-cycle
               [:click "button.anim"] run-animation])

(defn ^:export go []
  (so/log "going")
  (let [app (so/create ::app {:items (vec (repeat 100 0))})]
    (dom/replace-node (dom/by-id "app") app)))

