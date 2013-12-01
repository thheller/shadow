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
    (log "wtf" :yo {:crazy 'yeah})
    
    (go (loop []
          (anim/transition 2000 {container [[:font-size "1em" "3em" "linear"]]})
          (<! (anim/transition 1000 {box [[:padding-left "0px" "200px" "linear"]]}))
          (<! (anim/transition 1000 {box [[:padding-top "0px" "200px" "linear"]
                                          [:padding-left "200px" "0px" "linear"]]}))

          (anim/transition 2000 {container [[:font-size "3em" "1em" "linear"]]})
          (<! (anim/transition 1000 {box [[:padding-left "0px" "200px" "linear"]]}))
          (<! (anim/transition 1000 {box [[:padding-left "200px" "0px" "linear"]
                                          [:padding-top "200px" "0px" "linear"]]}))

          (recur)))))

(ns-ready)

