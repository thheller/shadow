(ns shadow.ui.position
  (:require [shadow.dom :as dom]))

(defn anchored [el anchor side top]
  (let [{:keys [w h]} (dom/get-size el)
        {:keys [x y]} (dom/get-position anchor)
        {anchor-w :w anchor-h :h} (dom/get-size anchor)]

    (cond
     (and (= side :right) (= top :center))
     (dom/set-style el {:left (str (+ anchor-w x 10) "px")
                        :top (str (+ (- y (/ h 2)) (/ anchor-h 2)) "px")})
     :else
     (throw (ex-info "dont know how to move to" {:side side :top top})))
    ))
