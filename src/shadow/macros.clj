(ns shadow.macros
  (:require [clojure.walk :as walk]
            [clojure.string :as str]))

(defmacro js-global [sym]
  (list 'js* (str sym)))

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


(defn reduce2 [f val coll]
  (loop [coll coll
         result val]
    (if (empty? coll)
      result
      (let [[a b] (take 2 coll)]
        (recur (drop 2 coll) (f result a b))))
    ))

(defmacro wait [wait-let & body]
  ;; if more than 1 result pair, combine and wait for all, otherwise just wait for the one
  (if (= 2 (count wait-let))
    (let [[value result] wait-let]
      `(goog.result/waitOnSuccess ~result (fn [~value]
                                            ~@body
                                            )))
    ;; this really needs a rewrite, first macro ever sure is ugly now
    (let [results (reduce2 #(conj %1 [%2 (gensym)] %3) [] wait-let)
          result-let (reduce2 (fn [result [_ key] value]
                                (conj result key value))
                              []
                              results)
          result-names (reduce2 (fn [result [_ key] _]
                                  (conj result key))
                                []
                                results)
          value-let (reduce2 (fn [result [orig alias] _]
                               (conj result orig `(.getValue ~alias)))
                             []
                             results)]
      `(let [~@result-let
             combo# (goog.result/combine ~@result-names)]
         (goog.result/waitOnSuccess
          combo#
          (fn [dummy#]
            (let [~@value-let]
              ~@body))))
      )))


(defn parse-tag [spec]
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


(defn replace-dom-vectors [form]
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

;; would be fancy but needs more thought
(defmacro domfn [& body]
  (let [fn-body (replace-dom-vectors body)]
    `(fn ~@fn-body)
    ))
