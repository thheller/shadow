(ns shadow.markup.css.impl.gen
  (:require [clojure.string :as str])
  #?(:cljs
     (:import [goog.string StringBuffer])))

(defprotocol IElement
  (el-selector [x])
  (el-type [x])
  (el-css [x env]))

(defprotocol IStyleGen
  (gen-css [x writer tag class]))

(declare map->root)

(defn generate-css
  [env el]
  (let [rule
        (el-css el env)

        tag
        (el-type el)

        class
        (el-selector el)

        sb
        #?(:cljs
           (StringBuffer.)
           :clj
           (StringBuilder.)) ;; StringBuffer synchronized in java

        rule
        (if (map? rule)
          (map->root rule)
          rule)]

    (gen-css rule sb tag class)
    (.toString sb)
    ))

(defn css-value-to-str [key value]
  (cond
    (string? value)
    value

    (number? value)
    (if (zero? value)
      "0"
      (case key
        :flex (str value)
        :font-weight (str value)
        :z-index (str value)
        :opacity (str value)
        (str value "px")))

    (vector? value)
    (let [[t r b l]
          value]
      (->> value
           (map #(css-value-to-str key %))
           (str/join " "))
      )))

(defn -write-css-attrs [writer attrs]
  (reduce-kv
    (fn [_ key value]
      (doto writer
        (.append "\n  ")
        (.append (name key))
        (.append ": ")
        (.append (css-value-to-str key value))
        (.append ";")))
    nil
    attrs))

(defn -write-rule [writer selector attrs]
  (when (seq attrs)
    (doto writer
      (.append selector)
      (.append " {")
      (-write-css-attrs attrs)
      (.append "\n}\n\n"))))

(defn merge-selector [selector tag class]
  (cond
    (str/includes? selector "%")
    (-> selector
        (str/replace #"%" tag)
        (str/replace #"&" (str "." class)))

    :else
    (str/replace selector #"&" (str tag "." class))))

(deftype Rule [type selector attrs rules]
  IStyleGen
  (gen-css [this writer tag class]
    (case type
      :root
      (do (-write-rule writer (str tag "." class) attrs)
          (doseq [rule rules]
            (gen-css rule writer tag class)))

      :rule
      (let [selector (merge-selector selector tag class)]
        (-write-rule writer selector attrs)
        (doseq [rule rules]
          (gen-css rule writer tag class)))

      :group
      (do (doto writer
            (.append selector)
            (.append " {\n\n"))

          (-write-rule writer (str tag "." class) attrs)
          (doseq [rule rules]
            (gen-css rule writer tag class))

          (doto writer
            (.append "\n}\n\n")))
      )))

(defn rule? [x]
  (instance? Rule x))

(defn group* [query attrs rules]
  {:pre [(string? query)
         (map? attrs)
         (every? rule? rules)]}
  (Rule. :group query attrs rules))

(defn media [query attrs & rules]
  (group* (str "@media" query) attrs rules))

(defn rule [selector attrs]
  {:pre [(string? selector)
         (map? attrs)]}
  (when-not (str/includes? selector "&")
    (throw (ex-info "rules must contain & to place prefix (&:hover instead of :hover)" {:selector selector :attrs attrs})))

  (Rule. :rule selector attrs []))

(defn nested-rule [parent attrs]
  (let [selector
        (cond
          (satisfies? IElement parent)
          (str "." (el-selector parent) " &")

          (and (vector? parent)
               (= 1 (count parent))
               (and (satisfies? IElement (first parent))))
          (str "." (el-selector (first parent)) " &")

          (and (vector? parent)
               (= 2 (count parent)))

          (let [[el suffix] parent]
            (when-not (satisfies? IElement el)
              (throw (ex-info "need to be nested in IElement" {:parent parent :attrs attrs})))
            (str "." (el-selector el) suffix " &")))]
    (Rule. :rule selector attrs [])))

(defn root* [attrs rules]
  {:pre [(map? attrs)
         (every? rule? rules)]}
  (Rule. :root nil attrs rules))

(defn root [attrs & rules]
  (root* attrs rules))

(defn attrs-from-map [m]
  (reduce-kv
    (fn [m k v]
      (if-not (keyword? k)
        m
        (assoc m k v)))
    {}
    m))

(defn- no-nested-rules!
  [selector rules]
  (when (seq rules)
    (throw (ex-info "selector cannot have nested rules" {:selector selector :rules (map :selector rules)}))))

(defn rules-from-map [m]
  (->> m
       (keys)
       (filter (complement keyword?))
       (map
         (fn [selector]
           (let [v (get m selector)

                 sub-rules
                 (rules-from-map v)

                 attrs
                 (attrs-from-map v)]
             (cond
               (and (string? selector)
                    (str/starts-with? selector "@"))
               (group* selector attrs sub-rules)

               ;; FIXME: we could allow nesting but styles get messy at that point
               (string? selector)
               (do (no-nested-rules! selector sub-rules)
                   (rule selector attrs))

               (satisfies? IElement selector)
               (do (no-nested-rules! selector sub-rules)
                   (nested-rule selector attrs))

               (vector? selector)
               (do (no-nested-rules! selector sub-rules)
                   (nested-rule selector attrs))

               :else
               (throw (ex-info "invalid key in map" {:key selector :value v}))
               ))))
       (into [])))

(defn map->root [m]
  (root* (attrs-from-map m) (rules-from-map m)))

