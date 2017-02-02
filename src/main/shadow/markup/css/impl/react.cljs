(ns shadow.markup.css.impl.react
  (:require [clojure.string :as str]
            [shadow.markup.react :as html]
            [shadow.markup.css.impl.gen :as gen]
            [shadow.dom :as dom])
  )

(defonce env-ref (volatile! {}))

(defonce active-elements-ref
  (volatile! {}))

(defn set-env! [new-env]
  (when-not (empty? @active-elements-ref)
    (js/console.log "TBD regenerate styles" new-env @active-elements-ref))
  (vreset! env-ref new-env))


;; FIXME: determine performance of injecting a bunch of <style> nodes vs CSSStylesheet.addRule
;; not using goog.style since it now requires the safe-style stuff, which we don't need really
(defn style-container [id]
  (let [el (dom/by-id id)]
    (if-not (nil? el)
      (do (dom/reset el)
          el)
      (let [node (dom/build [:style {:id id :type "text/css"}])]
        (dom/insert-first (dom/query-one "HEAD") node)
        node))))

(defn inject-rule [el]
  (let [text
        (gen/generate-css @env-ref el)

        css-sel
        (gen/el-selector el)

        container
        (style-container css-sel)

        node
        (js/document.createTextNode text)]
    (.appendChild container node)
    ))

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
        (assoc :className className)
        (dissoc :classes)
        (html/convert-props))))

;; called from macro, to make things live-reload friendly
(defn forget-rule! [css-sel]
  (vswap! active-elements-ref dissoc css-sel))

(defn styled-element-invoke [el props children]
  ;; style-fn is replaced after it is run once
  (when (not (.-injected? el))
    (forget-rule! (gen/el-selector el))
    (inject-rule el)
    (set! (.-injected? el) true))

  (let [css-sel
        (gen/el-selector el)

        el-type
        (gen/el-type el)]
    (if (map? props)
      (html/create-element* el-type (merge-props-and-class props css-sel) children)
      (html/create-element* el-type #js {:className css-sel} (cons props children)))))

(deftype StyledElement
  [el-type css-sel style-fn ^:mutable injected?]
  gen/IElement
  (el-type [_]
    el-type)
  (el-selector [_]
    css-sel)
  (el-css [_ env]
    (style-fn env)
    ))

;; starting to regret this, having the macro generate a vararg would be simpler
;; but I want a way to access the css-sel on a styled element
;; so nested-rule can work
(extend-type StyledElement
  cljs.core/IFn
  (-invoke
    ([el props]
     (styled-element-invoke el props []))
    ([el props c1]
     (styled-element-invoke el props [c1]))
    ([el props c1 c2]
      (styled-element-invoke el props [c1 c2]))
    ([el props c1 c2 c3]
      (styled-element-invoke el props [c1 c2 c3]))
    ([el props c1 c2 c3 c4]
      (styled-element-invoke el props [c1 c2 c3 c4]))
    ([el props c1 c2 c3 c4 c5]
      (styled-element-invoke el props [c1 c2 c3 c4 c5]))
    ([el props c1 c2 c3 c4 c5 c6]
      (styled-element-invoke el props [c1 c2 c3 c4 c5 c6]))
    ([el props c1 c2 c3 c4 c5 c6 c7]
      (styled-element-invoke el props [c1 c2 c3 c4 c5 c6 c7]))
    ([el props c1 c2 c3 c4 c5 c6 c7 c8]
      (styled-element-invoke el props [c1 c2 c3 c4 c5 c6 c7 c8]))
    ([el props c1 c2 c3 c4 c5 c6 c7 c8 c9]
      (styled-element-invoke el props [c1 c2 c3 c4 c5 c6 c7 c8 c9]))
    ([el props c1 c2 c3 c4 c5 c6 c7 c8 c9 c10]
     (styled-element-invoke el props [c1 c2 c3 c4 c5 c6 c7 c8 c9 c10]))
    ([el props c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11]
     (styled-element-invoke el props [c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11]))
    ([el props c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12]
     (styled-element-invoke el props [c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12]))
    ([el props c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13]
     (styled-element-invoke el props [c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13]))
    ([el props c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14]
     (styled-element-invoke el props [c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14]))
    ([el props c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15]
     (styled-element-invoke el props [c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15]))
    ([el props c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16]
     (styled-element-invoke el props [c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16]))
    ))

