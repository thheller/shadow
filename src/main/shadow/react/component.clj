(ns shadow.react.component
  (:require
    [clojure.spec.alpha :as s]
    [clojure.string :as str]
    [cljs.analyzer :as ana]))

(s/def ::kv-pair
  (s/cat
    :key keyword?
    :value any?))

(s/def ::fn
  (s/and
    list?
    (s/cat
      :key symbol?
      :args vector?
      :body (s/* any?))))

(s/def ::attr
  (s/alt
    :kv-pair ::kv-pair
    :fn ::fn))

(s/def ::factory-def
  (s/cat
    :id simple-symbol?
    :attrs (s/+ ::attr)
    ))

;; wrapping everything in a delay so things can be removed by Closure if not used
(defmacro deffactory
  [name & kv-pairs]
  (assert (even? (count kv-pairs)))
  (let [type
        (keyword (str *ns*) (str name))

        cmp
        (-> (str name "$init")
            (symbol)
            (with-meta {:tag 'not-native}))

        cmp-map
        (-> (apply array-map kv-pairs)
            (assoc ::type type))]

    ;; FIXME: not proper to introduce a new var but takes care of some DCE issues
    `(do (def ~cmp
           (-> ~cmp-map
               (shadow.react.component/make-component)
               (delay)))

         (defn ~name [props# & children#]
           (shadow.react.component/create-element* (cljs.core/-deref ~cmp) props# children#))

         ;; React-interop, some things require access to the component constructor
         ;; (deffactory my-component ...) @my-component or @my-component$init
         (cljs.core/specify! ~name
           cljs.core/IDeref
           (~'-deref [x#]
             (cljs.core/-deref ~cmp)))

         (js/goog.object.set ~name "shadow$component" #(cljs.core/-deref ~cmp)))))

(defmacro defc
  [& body]
  (let [data (s/conform ::factory-def body)]
    (when (= ::s/invalid data)
      (s/explain ::factory-def body)
      (throw (ex-info "invalid defui body"
               (s/explain-data ::factory-def body))))

    (let [{:keys [id attrs]}
          data

          type
          (keyword (str *ns*) (name id))

          cmp
          (-> (str (name id) "$init")
              (symbol)
              (with-meta {:tag 'not-native}))

          add-ext
          (fn [cmp key]
            (let [ext-id (symbol (namespace key) "shadow-ext")]
              (update cmp ::extensions
                (fn [current]
                  (if (nil? current)
                    [ext-id]
                    (into [] (distinct) (conj current ext-id)))))))

          cmp-map
          (reduce
            (fn [cmp [attr-type attr]]
              (case attr-type
                :kv-pair
                (-> (assoc cmp (:key attr) (:value attr))
                    (add-ext (:key attr)))
                :fn
                (let [{:keys [key args body]} attr

                      ;; (comp/render [x y z] ...)
                      ;; must be resolved so we can turn it into
                      ;; :shadow.react.component/render
                      ;; (fn [x y z] ...)
                      ;; can't emit ::comp/render
                      ns-alias
                      (ana/resolve-ns-alias &env (namespace key) key)

                      ;; this might be a bad idea since it introduces a new local
                      fn-id
                      (symbol (str (namespace key) "$" (name key)))

                      fn-key
                      (keyword (name ns-alias) (name key))]

                  (-> cmp
                      (assoc fn-key `(fn ~fn-id ~args ~@body))
                      (add-ext key)
                      ))))
            {::type type}
            attrs)]

      `(do (def ~cmp
             (-> ~cmp-map
                 (shadow.react.component/make-component)
                 (delay)))

           (defn ~id [props# & children#]
             (shadow.react.component/create-element* (cljs.core/-deref ~cmp) props# children#))

           ;; React-interop, some things require access to the component constructor
           ;; (deffactory my-component ...) @my-component or @my-component$init
           (cljs.core/specify! ~id
             cljs.core/IDeref
             (~'-deref [x#]
               (cljs.core/-deref ~cmp)))

           (js/goog.object.set ~id "shadow$component" #(cljs.core/-deref ~cmp)))
      )))