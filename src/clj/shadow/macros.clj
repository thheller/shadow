(ns shadow.macros
  (:require [clojure.string :as str]))

(defmacro ^{:js-require 'shadow.api} ns-ready
  []
  `(shadow.api/ns-ready ~(str *ns*)))

(defmacro ^{:js-require 'shadow.object} log
  [& args]
  `(.log js/console ~@(for [arg args]
                        `(shadow.object/console-friendly ~arg))))

(defmacro ^{:js-require 'shadow.xhr} wait
  [wait-let & body]
  ;; if more than 1 result pair, combine and wait for all, otherwise just wait for the one
  (if (= 2 (count wait-let))
    (let [[value result] wait-let]
      `(shadow.xhr/result-wait-on-success ~result (fn [~value] ~@body)))

    (let [let-bindings (->> wait-let
                            (partition 2)
                            (map (fn [[name value]]
                                   (let [alias (gensym)]
                                     {:name name
                                      :value value
                                      :alias alias
                                      :get-value `(.getValue ~alias)
                                      }))))]
      `(let [~@(mapcat (juxt :alias :value) let-bindings)
             combo# (shadow.xhr/result-combine ~@(map :alias let-bindings))]
         (shadow.xhr/result-wait-on-success
          combo#
          (fn [dummy#]
            (let [~@(mapcat (juxt :name :get-value) let-bindings)]
              ~@body)))))))


;; EXPERIMENT, dont use
(defn- parse-tag [spec]
  (let [spec (name spec)
        fdot (.indexOf spec ".")
        fhash (.indexOf spec "#")]
    (cond
     (and (= -1 fdot) (= -1 fhash))
     [spec nil nil]

     (= -1 fhash)
     [(.substring spec 0 fdot)
      nil
      (str/replace (.substring spec (inc fdot)) #"\." " ")]

     (= -1 fdot)
     [(.substring spec 0 fhash)
      (.substring spec (inc fhash))
      nil]

     (> fhash fdot)
     (throw (str "cant have id after class?" spec))

     :else
     [(.substring spec 0 fhash)
      (.substring spec (inc fhash) fdot)
      (str/replace (.substring spec (inc fdot)) #"\." " ")])))


(defn- replace-dom-vectors [form]
  (cond
   (and (vector? form) (keyword? (first form)))
   (let [[tag-def & body] form
         [tag-name tag-id tag-classes] (parse-tag tag-def)]
     (concat (list 'shadow.dom/macro-node tag-name tag-id tag-classes)
             (map replace-dom-vectors body)))
   (seq? form) (apply list (map replace-dom-vectors form))
   :else
   form
   ))

(defmacro domfn [& body]
  (let [fn-body (replace-dom-vectors body)]
    `(fn ~@fn-body)
    ))

;; //would be fancy but needs more thought


;; a way to write js classes in cljs, not using it anymore ...
(def prototype-prop '-prototype)

(defmulti class-prototype (fn [class form] (first form)))

(defmethod class-prototype 'inherits [class [_ super]]
  (update-in class [:inherits] conj super))

(defmethod class-prototype 'property [class [_ name default-value]]
  (update-in class [:properties] assoc name default-value))

(defmethod class-prototype 'constructor [class [_ args & body]]
  (assoc class :constructor {:args args
                             :body body}))

(defmethod class-prototype 'method [class [_ name args & body]]
  (update-in class [:methods] conj {:name name
                                    :args args
                                    :body body}))

(defn call-super [this args super]
  ; TODO: Support args!
  `(.call ~super ~this))

(defn call-inherits [this super]
  `(goog/inherits ~this ~super))

(defn make-constructor [{:keys [constructor name inherits] :as class-spec}]
  (let [args (:args constructor)
        body (:body constructor)
        this (first args)
        args (into [] (rest args))]
    (list 'do
          (list 'js* "goog.provide('~{}')" name)
          ;; emit constructor commit, otherwise closure compiler will complain!
          (list 'js* "/**\n * @constructor\n */\n")
          `(defn ~name ~args
             (cljs.core/this-as
               ~this
;              ~@(map (partial call-super this args) inherits)
               ~@body
               ~this
               ))
          `(do
             ~@(map (partial call-inherits name) inherits))
          )))

(defn make-property [name [prop-name prop-default]]
  `(set! (.. ~name ~prototype-prop ~prop-name) ~prop-default))

(defn make-properties [{:keys [properties name] :as class-spec}]
  (concat (list 'do)
          (map (partial make-property name) properties)))

(defn make-method [class-name {:keys [name args body]}]
  (let [this (first args)
        args (into [] (rest args))
        prop-name (symbol (str "-" name))
        fn-name (symbol (str class-name "$" name))]
    `(set! (.. ~class-name ~prototype-prop ~prop-name)
           (fn ~fn-name ~args
             (cljs.core/this-as
               ~this
               ~@body)))))

(defn make-methods [{:keys [methods name]}]
  (concat (list 'do)
          (map (partial make-method name) methods)))

(defn make-class [class-name body]
 (reduce class-prototype
         {:name class-name
          :inherits []
          :methods []
          :properties {}}
         body))

(defmacro defclass [class-name & body]
  (let [class-spec (make-class class-name body)]
    (list 'do
      (make-constructor class-spec)
      (make-properties class-spec)
      (make-methods class-spec)
      )))

