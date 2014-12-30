(ns shadow.test-app
  (:require-macros [shadow.macros :refer (log ns-ready)])
  (:require [shadow.components :as c :refer-macros (defc)]
            [shadow.api :as api]
            [shadow.dom :as dom]))

(def root-c (atom nil))

(def my-h1 (c/h1 {:class "special"}))

(defc yo
  :dom (fn [this children]
         [c/div
          [my-h1 (:name @this)]
          children]))

(defn ^:export start [ref]
  (log "START")
  (.time js/console "construct")
  (let [root (c/construct [(yo {:name "thomas"})
                           [(c/div {:data-backwards "compatible"
                                    :class "title"})]
                           [c/h2 "hello"]
                           [c/h3 "world"]])]
    (.timeEnd js/console "construct")
    (reset! root-c root)
    (dom/insert-before ref root)))

(defn ^:export stop []
  (log "STOP")
  (when-let [r @root-c]
    (c/destroy! r)
    (reset! root-c nil)))

(ns-ready)
