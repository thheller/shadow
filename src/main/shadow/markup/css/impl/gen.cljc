(ns shadow.markup.css.impl.gen
  (:require [clojure.string :as str]))

(defprotocol IElement
  (el-selector [x])
  (el-type [x])
  (el-root [x env]))

(defprotocol IStyleGen
  (gen-css-rules [x tag class] "generate a sequence of strings (css-rules)"))

(declare map->root)

(defn css-rules [root tag class]
  (let [root
        (if (map? root)
          (map->root root)
          root)]

    (gen-css-rules root tag class)
    ))

(defn css-rules-for-el
  [env el]
  (let [root
        (el-root el env)

        tag
        (el-type el)

        class
        (el-selector el)]

    (css-rules root tag class)))

(def number-as-str-props
  #{:flex
    :font-weight
    :z-index
    :opacity})

(defn css-value-to-str [key value]
  (cond
    (string? value)
    value

    (number? value)
    (cond
      (zero? value)
      "0"

      (contains? number-as-str-props key)
      (str value)

      :else
      (str value "px"))

    (vector? value)
    (->> value
         (map #(css-value-to-str key %))
         (str/join " "))
    ))

(defn gen-rule-attrs [attrs]
  (reduce-kv
    (fn [s key value]
      (str s "\n  " (name key) ": " (css-value-to-str key value) ";"))
    ""
    attrs))

(defn gen-rule [selector attrs]
  ;; safe-guard against invalid rules
  {:pre [(seq attrs)
         (string? selector)
         (every? keyword? (keys attrs))]}

  (str selector " {"
       (gen-rule-attrs attrs)
       "\n}"))

(defn merge-selector [selector tag class]
  (cond
    (str/includes? selector "%")
    (-> selector
        (str/replace #"%" tag)
        (str/replace #"&" (str "." class)))

    :else
    (str/replace selector #"&" (str tag "." class))))

(deftype Rule [type selector attrs nested-rules]
  IStyleGen
  (gen-css-rules [this tag class]
    (case type
      :root
      (let [self
            (if (seq attrs)
              [(gen-rule (str tag "." class) attrs)]
              [])]

        (->> nested-rules
             (mapcat #(gen-css-rules % tag class))
             (into self)))

      :rule
      (let [self
            (if (seq attrs)
              (let [selector (merge-selector selector tag class)]
                [(gen-rule selector attrs)])
              [])]

        (->> nested-rules
             (mapcat #(gen-css-rules % tag class))
             (into self)))

      :group
      (let [nested
            (mapcat #(gen-css-rules % tag class) nested-rules)]

        (if (and (not (seq attrs))
                 (not (seq nested)))
          ;; group has not attrs or nested rules, don't generate a rule
          []
          ;; generate one rule that combines all other rules
          [(str selector " {\n"
                (when (seq attrs)
                  (gen-rule (str tag "." class) attrs))
                (when (seq nested)
                  (str "\n" (str/join "\n" nested)))
                "\n}")])))))

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

