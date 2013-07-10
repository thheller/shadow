(ns shadow.ui.position
  (:import goog.math.Box
           goog.math.Coordinate)
  (:require [shadow.dom :as dom]
            [goog.positioning :as pos]
            ))

(def corner-ids
  {:top-left 0
   :top-right 2
   :bottom-left 1
   :bottom-right 3
   :top-start 4
   :top-end 6
   :bottom-start 5
   :bottom-end 7})

(def overflow-bits
  {:ignore 0
   :adjust-x 1
   :fail-x 2
   :adjust-y 4
   :fail-y 8
   :resize-width 16
   :resize-height 32
   :adjust-x-except-offscreen 65
   :adjust-y-except-offscreen 132})


(defn ->corner [c]
  (or (get corner-ids c)
      (throw (ex-info "unsupported corner" {:corner c :supported (keys corner-ids)}))))

(defn ->coordinate [c]
  (cond
   (nil? c) nil
   (number? c) (Coordinate. c c)
   (vector? c) (Coordinate. (nth c 0) (nth c 1))
   (map? c) (Coordinate. (:x c 0) (:y c 0))
   :else (throw (ex-info "invalid coordinate" {:c c}))))

(defn ->box [c]
  (cond
   (nil? c) nil
   (number? c)
   (Box. c c c c)
   (vector? c)
   (condp = (count c)
     1 (let [x (nth c 0)]
         (Box. x x x x))
     2 (let [y (nth c 0)
             x (nth c 1)]
         (Box. y x y x))
     4 (Box. (nth c 0) (nth c 1) (nth c 2) (nth c 3))
     (throw (ex-info "invalid vector size for box" {:box c})))
   (map? c)
   (Box. (:top c 0)
         (:right c 0)
         (:bottom c 0)
         (:left c 0))
   :else
   (throw (ex-info "invalid box" {:box c}))
   ))

(defn ->overflow [keys]
  (reduce
   (fn [v key]
     (bit-or
      v
      (or (get overflow-bits key)
          (throw (ex-info "invalid overflow arg" {:keys keys :key key})))))
   0
   keys))

(defn anchored
  "position an element at an anchor
   corner is one of corner-ids
   offsets is x or [x y] (ints)
   margins is x [y x] [top right bottom left] (ints, css-style margin, padding)
   overflow is a set of overflow-bits keywords
   viewport nil or see margins"
  [anchor
   anchor-corner
   el
   el-corner
   offsets
   margins
   overflow
   preferred-size
   viewport]
  (pos/positionAtAnchor
   (dom/dom-node anchor)
   (->corner anchor-corner)
   (dom/dom-node el)
   (->corner el-corner)
   (->coordinate offsets)
   (->box margins)
   (->overflow overflow)
   (->box viewport)))
