(ns shadow.markup.css
  (:require [clojure.string :as str]
            [shadow.markup.css.impl.gen :as gen]
            [shadow.markup.hiccup.impl :as hiccup]))

(defn element*
  "use defstyled macro"
  [el-type el-selector style-fn]
  (hiccup/->StyledElement el-type el-selector style-fn))

(defmacro defstyled
  "(defstyled my-div :div
     [env]
     {:color \"red\"})"
  [el-name el-type args & body]
  {:pre [(symbol? el-name)
         (keyword? el-type)
         (vector? args)
         (= 1 (count args))]}

  (let [el-selector (gen/gen-el-selector *ns* el-name)]
    `(def ~(vary-meta el-name assoc :shadow/dom-fn true :shadow.markup.css/element true)
       (element*
         ~(name el-type)
         ~el-selector
         (fn ~args
           ~@body)))))

(defn root [attrs & rules]
  (gen/root* attrs rules))

(defn rule [selector attrs]
  (gen/rule selector attrs))

(defn nested-rule [parent attrs]
  (gen/nested-rule parent attrs))

;; for server side css generation
(defn- find-elements* [ns-sym]
  (if (qualified-symbol? ns-sym)
    ;; only one var
    (let [el-var (resolve ns-sym)]
      (when-not el-var
        (throw (ex-info (str "cannot find css element by name " ns-sym) {:sym ns-sym})))
      [@el-var])
    ;; symbols without ns yield all elements defined in ns
    (->> (ns-publics ns-sym)
         (vals)
         (filter #(-> % meta :shadow.markup.css/element))
         (map deref))
    ))

(defn require-elements!
  [ns-symbols]
  (doseq [ns-sym
          (->> ns-symbols
               (map (fn [ns-sym]
                      (if (simple-symbol? ns-sym)
                        ns-sym
                        (symbol (namespace ns-sym)))))
               (distinct))]

    (when (nil? (find-ns ns-sym))
      (require ns-sym)))

  ns-symbols)

(defn find-elements
  "given a list of symbols find all css elements
   namespaced symbols should resolve to a single element
   simple symbols are treated as an ns and yield all defined elements in that ns"
  [ns-symbols]
  (into [] (mapcat find-elements*) ns-symbols))

(defn generate-css-for-elements
  [env elements]
  (->> elements
       (mapcat #(gen/css-rules-for-el env %))
       (str/join "\n")))

(comment
  (defstyled bar :div
    [_]
    {:color "bar"})

  (defstyled foo :div
    [_]
    {:color "foo"
     bar
     {:color "red"}})

  ;; this could be used to generate .css files to be used via <link .../>
  (println
    (->> '[shadow.markup.css]
         ;; (require-elements!)
         ;; can be skipped if already required in ns form
         (find-elements)
         (generate-css-for-elements {})
         ))

  ;; this could be a strategy to produce only the css used on a given page
  (let [el1
        (element* "div" "outer" (fn [x] {:border "1px solid red"}))

        el2
        (element* "div" "inner" (fn [x] {:background-color "green"}))

        used
        (volatile! #{})

        html
        (binding [hiccup/*used-elements* used]
          (el1 {:data-x "yo" :data-y true}
            ;; meh on the assumption of hiccup
            [:h1 "hello world"]
            (el2 {})))]

    ;; <style>
    (println (generate-css-for-elements {} @used))
    ;; </style>
    (println)
    ;; <html>
    (println html)
    ;; </html>
    ))