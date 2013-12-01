(ns shadow.tests
  (:require-macros [shadow.macros :refer (log ns-ready)]
                   [cljs.core.async.macros :refer (go)])
  (:require [shadow.api :as api]
            [shadow.object :as so]
            [shadow.dom :as dom]
            [shadow.animate :as anim]
            ))

(defn ^:export dance [where]
  (let [container (dom/build [:div.container {:style "height: 300px; overflow: hidden;"}
                              [:div.box "hello there"]])
        box (dom/query-one ".box" container)]

    (dom/replace-node where container)
    
    (go (loop []
          (anim/start 2000 {container (anim/transition :font-size "1em" "3em" "linear")})
          (<! (anim/start 1000 {box (anim/transition :padding-left "0px" "200px" "linear")}))
          (<! (anim/start 1000 {box (anim/combine
                                     (anim/transition :padding-top "0px" "200px" "linear")
                                     (anim/transition :padding-left "200px" "0px" "linear"))}))

          (anim/start 2000 {container (anim/transition :font-size "3em" "1em" "linear")})
          (<! (anim/start 1000 {box (anim/transition :padding-left "0px" "200px" "linear")}))
          (<! (anim/start 1000 {box (anim/combine
                                     (anim/transition :padding-left "200px" "0px" "linear")
                                     (anim/transition :padding-top "200px" "0px" "linear"))}))

          (recur)))))

(ns-ready)

