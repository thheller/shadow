(ns shadow.material.ripple
  {:doc "material design inspired ripple"
   :url "https://www.polymer-project.org/docs/elements/paper-elements.html#paper-ripple"}
  (:require-macros [cljs.core.async.macros :refer (go alt!)])
  (:require [cljs.core.async :as async]
            [shadow.components :as sc :refer (defc $)]
            [shadow.animate :as anim]
            [shadow.html :as html]
            [shadow.dom :as dom]))

;; FIXME: very naive implementation, should probably wrap element
;; FIXME: since it is not wrapping the element it won't move if the element does
;; FIXME: paper does 2 waves
;; FIXME: paper optionally moves wave1 to center of element
;; FIXME: paper does custom animation through rAF
;; FIXME: this is really bad performance wise, creating/destroying objects all the time
;; FIXME: although that doesn't matter cause you have to click really fast to notice

(defc sm-ripple
  :dom (fn [this _]
         ($ (html/div {:class "sm-ripple"}))))

(defc sm-wave
  :dom (fn [this _]
         ($ (html/div {:class "sm-wave-1"}))
         ))

(defn start! [parent e el]
  (let [{:keys [w h]} (dom/get-size el)
        {:keys [x y]} (dom/get-position el)
        container (sc/construct parent sm-ripple)
        wave (sc/construct container sm-wave)
        ex (.-pageX e)
        ey (.-pageY e)
        grow (anim/setup 500 {wave (anim/combine
                                    (anim/transition :opacity "0.8" "0.1" "ease-out")
                                    (anim/transition :transform "scale(1)" "scale(25)" "ease-out"))})]

    (dom/set-style container {:width (dom/px w)
                              :height (dom/px h)
                              :top (dom/px y)
                              :left (dom/px x)
                              :position "absolute"
                              :overflow "hidden"
                              :z-index "1"})

    (dom/set-style wave {:position "relative"
                         :width (dom/px 6)
                         :height (dom/px 6)
                         :left (-> ex (- x) dom/px)
                         :top (-> ey (- y) dom/px)
                         :border-radius (dom/pct 50)})
    
    (anim/init! grow)
    (dom/append container wave)
    (dom/append container)
    
    (let [mouse-up (dom/event-chan js/document :mouseup identity true)]
      (go (<! (anim/continue! grow))
          (alt!
            mouse-up
            ([_] (sc/destroy! container))

            container
            ([_] :already-dead))))))

(defn for-element [parent]
  (fn [el scope]
    (dom/on el :mousedown (fn [e el]
                            (start! parent e el)))))
