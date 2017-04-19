(ns shadow.markup.react.impl.interop
  (:require [goog.object :as gobj]))

(def ^{:private true} element-marker
  (-> (js/React.createElement "div" nil)
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

;; called from macro
;; type is a string
;; props is a js object
;; children is an array
(defn create-element* [type props children]
  (gobj/set
    props
    "children"
    (let [c (count children)]
      (condp = c
        0 nil
        1 (first children)
        children)))

  ;; FIXME: create low level version that directly creates a JS object instead
  ;; createElement needlessly copies props to extra ref/key
  (js/React.createElement type props))

;; fallback if the macro didn't do this
(defn create-element [type args]
  (let [[head & tail] args]
    (cond
      (map? head)
      (create-element* type (convert-props head) tail)

      (nil? head)
      (create-element* type #js {} tail)

      (element? head)
      (create-element* type #js {} args)

      (object? head)
      (create-element* type head tail)

      :else
      (create-element* type #js {} args)
      )))

