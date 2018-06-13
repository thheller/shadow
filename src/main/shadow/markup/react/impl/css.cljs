(ns shadow.markup.react.impl.css
  (:require [clojure.string :as str]
            [shadow.markup.react.impl.interop :as interop]
            [shadow.markup.css.impl.gen :as gen]
            [shadow.markup.env :as env]
            [shadow.dom :as dom]
            ["react" :as react]))

(defonce env-ref (volatile! {}))

(defonce active-elements-ref
  ;; FIXME: is sorting useful?
  ;; {classname-of-el el-instance}
  ;; keyed by classname since live-reloading will create a new el-instance
  ;; which can't remove the old instance since it is not equal
  (volatile! (sorted-map)))

(defonce style-cache-ref
  (volatile! {}))

(defn get-rules-for-el [env el]
  (let [name (gen/el-selector el)]
    (or (get @style-cache-ref name)
        (let [css
              (gen/css-rules-for-el env el)]
          (vswap! style-cache-ref assoc name css)
          css
          ))))

(defn style-container []
  (let [styles-container-id "shadow-markup-styles"]
    (or (dom/by-id styles-container-id)
        (let [node (dom/build [:style {:id styles-container-id :type "text/css"}])]
          (dom/append js/document.head node)
          node))))

(defn insert-styles!
  "inserts rules for a single el using sheet.insertRule"
  [el]
  (let [container
        (style-container)

        n
        (.. container -sheet -rules -length)

        rules
        (get-rules-for-el @env-ref el)]

    (dotimes [i (count rules)]
      (.. container -sheet (insertRule (nth rules i) (+ n i))))
    ))

(defn regenerate-styles!
  "generates a css string and replace the textContent of the <style> container"
  []
  (let [env
        @env-ref

        styles
        (->> @active-elements-ref
             (vals)
             (mapcat #(get-rules-for-el env %))
             (str/join "\n"))

        container
        (style-container)]

    (set! (.-textContent container) styles)))

(defn set-env! [new-env]
  (vreset! env-ref new-env)
  (vreset! style-cache-ref {})

  (when-not (empty? @active-elements-ref)
    (regenerate-styles!)))

(defn inject-rules! [el]
  (let [selector (gen/el-selector el)]
    (vswap! style-cache-ref dissoc selector)
    (vswap! active-elements-ref assoc selector el)

    ;; when using many elements for the first time
    ;; each element will cause a re-parse of all previously generated css rules
    ;; since we replace the textContent of the <style> element
    ;; the better option would be to use style.sheet.insertRule
    ;; but that has the effect of not being able to modify styles in devtools
    ;; which is annoying, so we only do it when env/DEBUG is false

    (if ^boolean env/DEBUG
      (regenerate-styles!)
      (try
        (insert-styles! el)
        (catch :default e
          (js/console.error "insert-styles failed" e el)
          ;; fallback in case insertRule fails
          (regenerate-styles!))))))

(defn check-conflicting-props! [{:keys [class className classes] :as props}]
  (let [total
        (cond-> 0
          class
          (inc)
          className
          (inc)
          classes
          (inc))]

    (when (> total 1)
      (throw (ex-info "conflicting props, can only have one of: class className classes" props)))
    ))

(defn merge-props-and-class [props class]
  (when ^boolean env/DEBUG
    (check-conflicting-props! props))

  (let [class-from-props
        (or (:class props)
            (:className props)
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
        (dissoc :class :classes)
        (assoc :className className)
        (interop/convert-props))))

(defn styled-element-invoke [^clj el props ^js args]
  (when (not (.-injected? el))
    (inject-rules! el)
    (set! (.-injected? el) true))

  (let [css-sel (gen/el-selector el)
        el-type (gen/el-type el)]

    (if (map? props)
      (.unshift args (merge-props-and-class props css-sel))
      (do (.unshift args props)
          (.unshift args #js {:className css-sel})))

    (.unshift args el-type)
    (.apply react/createElement nil args)))

(deftype StyledElement
  [el-type css-sel style-fn ^:mutable injected?]
  gen/IElement
  (el-type [_]
    el-type)
  (el-selector [_]
    css-sel)
  (el-root [_ env]
    (style-fn env))

  IFn
  (-invoke [el props]
    (styled-element-invoke el props #js []))
  (-invoke [el props c1]
    (styled-element-invoke el props #js [c1]))
  (-invoke [el props c1 c2]
    (styled-element-invoke el props #js [c1 c2]))
  (-invoke [el props c1 c2 c3]
    (styled-element-invoke el props #js [c1 c2 c3]))
  (-invoke [el props c1 c2 c3 c4]
    (styled-element-invoke el props #js [c1 c2 c3 c4]))
  (-invoke [el props c1 c2 c3 c4 c5]
    (styled-element-invoke el props #js [c1 c2 c3 c4 c5]))
  (-invoke [el props c1 c2 c3 c4 c5 c6]
    (styled-element-invoke el props #js [c1 c2 c3 c4 c5 c6]))
  (-invoke [el props c1 c2 c3 c4 c5 c6 c7]
    (styled-element-invoke el props #js [c1 c2 c3 c4 c5 c6 c7]))
  (-invoke [el props c1 c2 c3 c4 c5 c6 c7 c8]
    (styled-element-invoke el props #js [c1 c2 c3 c4 c5 c6 c7 c8]))
  (-invoke [el props c1 c2 c3 c4 c5 c6 c7 c8 c9]
    (styled-element-invoke el props #js [c1 c2 c3 c4 c5 c6 c7 c8 c9]))
  (-invoke [el props c1 c2 c3 c4 c5 c6 c7 c8 c9 c10]
    (styled-element-invoke el props #js [c1 c2 c3 c4 c5 c6 c7 c8 c9 c10]))
  (-invoke [el props c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11]
    (styled-element-invoke el props #js [c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11]))
  (-invoke [el props c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12]
    (styled-element-invoke el props #js [c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12]))
  (-invoke [el props c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13]
    (styled-element-invoke el props #js [c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13]))
  (-invoke [el props c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14]
    (styled-element-invoke el props #js [c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14]))
  (-invoke [el props c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15]
    (styled-element-invoke el props #js [c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15]))
  (-invoke [el props c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16]
    (styled-element-invoke el props #js [c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16]))
  ;; FIXME: add more
  )
