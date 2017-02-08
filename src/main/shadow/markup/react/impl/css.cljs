(ns shadow.markup.react.impl.css
  (:require [clojure.string :as str]
            [shadow.markup.react.impl.interop :as interop]
            [shadow.markup.css.impl.gen :as gen]
            [shadow.dom :as dom]
            [goog.async.nextTick]))

(defonce env-ref (volatile! {}))

(defonce active-elements-ref
  ;; FIXME: is sorting useful?
  ;; {classname-of-el el-instance}
  ;; keyed by classname since live-reloading will create a new el-instance
  ;; which can't remove the old instance since it is not equal
  (volatile! (sorted-map)))

(defonce style-cache-ref
  (volatile! {}))

(defn get-css-for-el [env el]
  (let [name (gen/el-selector el)]
    (or (get @style-cache-ref name)
        (let [css
              (->> (gen/css-rules-for-el env el)
                   (str/join "\n"))]
          (vswap! style-cache-ref assoc name css)
          css
          ))))

(defonce regen-pending-ref (volatile! false))

(defn style-container []
  (let [styles-container-id "shadow-markup-styles"]
    (or (dom/by-id styles-container-id)
        (let [node (dom/build [:style {:id styles-container-id :type "text/css"}])]
          (dom/append js/document.head node)
          node))))

(defonce flush-id-seq (volatile! 0))

(defn flush-styles!
  "user can call this early (ie. after calling ReactDOM.render)"
  []
  (let [flush-id
        (vswap! flush-id-seq inc)

        label-start
        (str "generate/start#" flush-id)

        label-finish
        (str "generate/finish#" flush-id)

        mark?
        (and js/performance
             js/performance.mark)]

    (when mark?
      (js/performance.mark label-start))

    (let [env
          @env-ref

          styles
          (->> @active-elements-ref
               (vals)
               (map #(get-css-for-el env %))
               (str/join "\n"))

          container
          (style-container)]

      (set! (.-textContent container) styles))

    (when mark?
      (js/performance.mark label-finish)
      (js/performance.measure (str "shadow-markup-styles/flush#" flush-id) label-start label-finish)))

  (vreset! regen-pending-ref false))

(defn maybe-flush-styles! []
  (when @regen-pending-ref
    (flush-styles!)))

(defn regenerate-styles! []
  ;; FIXME: FOUC when doing it async, not really a viable strategy.
  #_(when-not @regen-pending-ref
      (vreset! regen-pending-ref true)
      (js/goog.async.nextTick maybe-flush-styles!))

  (flush-styles!))

(defn set-env! [new-env]
  (vreset! env-ref new-env)
  (vreset! style-cache-ref {})

  (when-not (empty? @active-elements-ref)
    (regenerate-styles!)))

(defn inject-rules! [el]
  (let [selector (gen/el-selector el)]
    (vswap! style-cache-ref dissoc selector)
    (vswap! active-elements-ref assoc selector el)
    (regenerate-styles!)))

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
  (check-conflicting-props! props)

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

(defn styled-element-invoke [el props children]
  ;; style-fn is replaced after it is run once
  (when (not (.-injected? el))
    (inject-rules! el)
    (set! (.-injected? el) true))

  (let [css-sel
        (gen/el-selector el)

        el-type
        (gen/el-type el)]
    (if (map? props)
      (interop/create-element* el-type (merge-props-and-class props css-sel) children)
      (interop/create-element* el-type #js {:className css-sel} (cons props children)))))

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
    (styled-element-invoke el props []))
  (-invoke [el props c1]
    (styled-element-invoke el props [c1]))
  (-invoke [el props c1 c2]
    (styled-element-invoke el props [c1 c2]))
  (-invoke [el props c1 c2 c3]
    (styled-element-invoke el props [c1 c2 c3]))
  (-invoke [el props c1 c2 c3 c4]
    (styled-element-invoke el props [c1 c2 c3 c4]))
  (-invoke [el props c1 c2 c3 c4 c5]
    (styled-element-invoke el props [c1 c2 c3 c4 c5]))
  (-invoke [el props c1 c2 c3 c4 c5 c6]
    (styled-element-invoke el props [c1 c2 c3 c4 c5 c6]))
  (-invoke [el props c1 c2 c3 c4 c5 c6 c7]
    (styled-element-invoke el props [c1 c2 c3 c4 c5 c6 c7]))
  (-invoke [el props c1 c2 c3 c4 c5 c6 c7 c8]
    (styled-element-invoke el props [c1 c2 c3 c4 c5 c6 c7 c8]))
  (-invoke [el props c1 c2 c3 c4 c5 c6 c7 c8 c9]
    (styled-element-invoke el props [c1 c2 c3 c4 c5 c6 c7 c8 c9]))
  (-invoke [el props c1 c2 c3 c4 c5 c6 c7 c8 c9 c10]
    (styled-element-invoke el props [c1 c2 c3 c4 c5 c6 c7 c8 c9 c10]))
  (-invoke [el props c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11]
    (styled-element-invoke el props [c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11]))
  (-invoke [el props c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12]
    (styled-element-invoke el props [c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12]))
  (-invoke [el props c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13]
    (styled-element-invoke el props [c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13]))
  (-invoke [el props c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14]
    (styled-element-invoke el props [c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14]))
  (-invoke [el props c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15]
    (styled-element-invoke el props [c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15]))
  (-invoke [el props c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16]
    (styled-element-invoke el props [c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16]))
  ;; FIXME: add more
  )

(comment
  ;; crazy idea I had for :advanced mode
  ;; CollapseProperties renames all defstyled to global names
  ;; these end up on js/window (with everything else ...)
  ;; we can just map over them and check for the IElement protocol
  ;; so we get a collection of all defstyled elements that survived
  ;; :advanced and could be used to inject all styles in one go.
  ;; but given the insane amount of variables on js/window
  ;; this turns out to be quite costly also not sure I actually want to rely on this
  ;; as this can easily mess things up, and won't work with :output-wrapper

  ;; still a fun idea

  ;; produces a warning on access
  (def blacklist #{"webkitStorageInfo"})

  (defn ^:export find-elements []
    (js/console.time "find-elements")
    (let [src
          js/window

          surviving-elements
          (->> (js/goog.object.getKeys src)
               (array-seq)
               (remove blacklist)
               (map #(js/goog.object.get src %))
               (filter #(implements? gen/IElement %))
               (into []))]

      (js/console.timeEnd "find-elements")
      (js/console.log "find-elements" (into-array surviving-elements)))))