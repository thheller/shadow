(ns shadow.inspect
  (:require [shadow.dom :as dom]
            [shadow.object :as so]))

(defprotocol Inspectable
  (-inspect [this]))

(defn do-inspect [object]
  (cond
   (nil? object) [:div.inspect-nil "nil"]
   (satisfies? Inspectable object) (-inspect object)
   :else
   (do
     (.log js/console "not inspectable" object)
     [:table.inspect-missing
      [:tr
       [:td "NOT INSPECTABLE:" (str (type object))]
       [:td (pr-str object)]]])
   ))

(defn do-inspect-map [this]
  [:table.inspect-map
   [:col {:width "10"}]
   [:col]
   (for [[k v] (sort-by #(str (first %)) (seq this))]
     [:tr
      [:td.key (do-inspect k)]
      [:td.value (do-inspect v)]])
   ])

(defn do-inspect-seq [this]
  [:ol.inspect-seq
   (for [v this]
     [:li (do-inspect v)]
     )])

(extend-protocol Inspectable
  js/String
  (-inspect [this]
    (if (keyword? this)
      [:span.inspect-keyword [:nobr (str this)]]
      [:span.inspect-string this]))

  number
  (-inspect [this] [:span.inspect-number this])

  boolean
  (-inspect [this] [:span.inspect-boolean (str this)])

  js/Date
  (-inspect [this] [:span.inspect-date (str this)])

  js/Function
  (-inspect [this] [:span.inspect-function (pr-str this)])

  PersistentHashMap
  (-inspect [this] (do-inspect-map this))

  PersistentArrayMap
  (-inspect [this] (do-inspect-map this))

  ObjMap
  (-inspect [this] (do-inspect-map this))

  PersistentVector
  (-inspect [this] (do-inspect-seq this))

  List
  (-inspect [this] (do-inspect-seq this))

  PersistentHashSet
  (-inspect [this] [:ul.inspect-set
                    (for [v this]
                      [:li (do-inspect v)]
                      )])
  )

(defn inspect [object]
  (dom/build [:div.inspect (do-inspect object)]))
