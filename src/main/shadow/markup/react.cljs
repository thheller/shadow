(ns shadow.markup.react
  "EXPERIMENTAL - DO NOT USE"
  (:refer-clojure :exclude [for map meta time])
  (:require-macros [shadow.markup.react :as m])
  ;; (:require [cljsjs.react]) ;; assume this is provided somewhere else
  )

(defn for [& args]
  (throw (ex-info "shadow.markup.react/for is a macro" {})))

(def element-marker
  (-> (js/React.createElement "div" nil)
      (js/goog.object.get "$$typeof")))

(defn element? [x]
  (and (object? x)
       (= element-marker (js/goog.object.get x "$$typeof"))))

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
  (js/goog.object.set
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

      (object? head)
      (create-element* type head tail)

      :else
      (create-element* type #js {} args)
      )))

(defn $ [type & args]
  (create-element type args))

(defn with-inner-html [html props]
  ;; what a load of crap ...
  (let [html #js {"__html" html}]
    (js/goog.object.set props "dangerouslySetInnerHTML" html))
  props)

(m/define-elements)