(ns shadow.markup.react.impl.interop
  (:require [goog.object :as gobj]
            ["react" :as react]))

(def ^{:private true} element-marker
  (-> (react/createElement "div" nil)
      (gobj/get "$$typeof")))

(defn element? [x]
  (and (object? x)
       (= element-marker (gobj/get x "$$typeof"))))

(defn convert-props [props]
  (cond
    (nil? props)
    #js {}
    (map? props)
    (clj->js props)
    :else
    props))

(extend-type js/Symbol
  IPrintWithWriter
  (-pr-writer [sym w o]
    (-write w (.toString sym))))

;; called from macro
;; react v16 is really picky, the old direct .children prop trick no longer works
(defn create-element* [arr]
  {:pre [(array? arr)]}
  (.apply react/createElement nil arr))

(defn arr-append* [arr x]
  (.push arr x)
  arr)

(defn arr-append [arr tail]
  (reduce arr-append* arr tail))

;; fallback if the macro didn't do this
(defn create-element [type args]
  (let [[head & tail] args]
    (cond
      (map? head)
      (create-element*
        (doto #js [type (convert-props head)]
          (arr-append tail)))

      (nil? head)
      (create-element*
        (doto #js [type nil]
          (arr-append tail)))

      (element? head)
      (create-element*
        (doto #js [type nil]
          (arr-append args)))

      (object? head)
      (create-element*
        (doto #js [type head]
          (arr-append tail)))

      :else
      (create-element*
        (doto #js [type nil]
          (arr-append args)))
      )))

