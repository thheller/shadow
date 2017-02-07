(ns shadow.markup.css.impl.server
  (:require [shadow.markup.css.impl.gen :as gen]
            [clojure.string :as str]
            [hiccup.util :as util]
            [hiccup.compiler :as comp])
  (:import (clojure.lang IFn)))

(def ^:dynamic *used-elements* nil)

;; this is directly from hiccup.compiler, it is private so it is copied here
;; https://github.com/weavejester/hiccup/blob/master/src/hiccup/compiler.clj
;; modified so we don't sort and apply

;; COPY

(defn- render-style-map [value]
  (->> value
       (map (fn [[k v]] (str (util/as-str k) ":" v ";")))
       (str/join "")))

(defn- render-attr-value [value]
  (cond
    (map? value)
    (render-style-map value)
    (sequential? value)
    (str/join " " value)
    :else
    value))

(defn- xml-attribute [name value]
  (str " " (util/as-str name) "=\"" (util/escape-html (render-attr-value value)) "\""))

(defn- render-attribute [name value]
  (cond
    (true? value)
    (str " " (util/as-str name))
    (not value)
    ""
    :else
    (xml-attribute name value)))

(defn render-attr-map
  "Render a map of attributes."
  [attrs]
  (reduce-kv #(str %1 (render-attribute %2 %3)) "" attrs))

;; // COPY

(defn merge-props-and-class [props class]
  ;; FIXME: should warn if :classes and :className is present
  (let [class-from-props
        (or (:className props)
            (when-let [classes (:classes props)]
              (if (map? classes)
                ;; {:selected boolean-ish}
                (->> classes
                     (keys)
                     (filter
                       (fn [key]
                         (get classes key)))
                     (map name)
                     (str/join " "))

                ;; [(when x "selected") ...]
                (->> classes
                     (remove nil?)
                     (str/join " ")))))

        className
        (if (nil? class-from-props)
          class
          (str class " " class-from-props))]

    (-> props
        (assoc :class className)
        (dissoc :classes :className))))

(defn gen-nested-html [children]
  (reduce
    (fn [s child]
      ;; FIXME: I want to drop the assumption that we are within hiccup
      ;; but a large part of my templates are still hiccup
      ;; could maybe alter-var-root the gen-nested-html to remove the reliance on hiccup
      (str s (comp/render-html child)))
    ""
    children))

(defn gen-html [el props children]
  ;; FIXME: be more like hiccup and let props be optional
  {:pre [(map? props)]}
  (let [tag
        (gen/el-type el)

        props
        (merge-props-and-class props (gen/el-selector el))

        child-html
        (gen-nested-html children)]

    (when *used-elements*
      (vswap! *used-elements* conj el))

    (str "<" tag (render-attr-map props) ">"
         child-html
         "</" tag ">")))

(deftype StyledElement [el-type el-selector style-fn]
  gen/IElement
  (el-type [_]
    el-type)
  (el-selector [_]
    el-selector)
  (el-root [_ env]
    (style-fn env))

  IFn
  (invoke [el]
    (gen-html el {} []))
  (invoke [el props]
    (gen-html el props []))
  (invoke [el props c1]
    (gen-html el props [c1]))
  (invoke [el props c1 c2]
    (gen-html el props [c1 c2]))
  (invoke [el props c1 c2 c3]
    (gen-html el props [c1 c2 c3]))
  (invoke [el props c1 c2 c3 c4]
    (gen-html el props [c1 c2 c3 c4]))
  (invoke [el props c1 c2 c3 c4 c5]
    (gen-html el props [c1 c2 c3 c4 c5]))
  (invoke [el props c1 c2 c3 c4 c5 c6]
    (gen-html el props [c1 c2 c3 c4 c5 c6]))
  (invoke [el props c1 c2 c3 c4 c5 c6 c7]
    (gen-html el props [c1 c2 c3 c4 c5 c6 c7]))
  (invoke [el props c1 c2 c3 c4 c5 c6 c7 c8]
    (gen-html el props [c1 c2 c3 c4 c5 c6 c7 c8]))
  (invoke [el props c1 c2 c3 c4 c5 c6 c7 c8 c9]
    (gen-html el props [c1 c2 c3 c4 c5 c6 c7 c8 c9]))
  (invoke [el props c1 c2 c3 c4 c5 c6 c7 c8 c9 c10]
    (gen-html el props [c1 c2 c3 c4 c5 c6 c7 c8 c9 c10]))
  (invoke [el props c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11]
    (gen-html el props [c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11]))
  (invoke [el props c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12]
    (gen-html el props [c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12]))
  (invoke [el props c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13]
    (gen-html el props [c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13]))
  (invoke [el props c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14]
    (gen-html el props [c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14]))
  (invoke [el props c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15]
    (gen-html el props [c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15]))
  (invoke [el props c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16]
    (gen-html el props [c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16]))
  (applyTo [el s]
    (gen-html el (first s) (rest s))))





