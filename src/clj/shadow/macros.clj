(ns shadow.macros
  (:require [clojure.string :as str]))

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


(defmacro define-node-factories [syms]
  `(do ~@(for [sym syms]
           `(def ~sym (shadow.components/ElementFactory.
                       #(shadow.components/dom-element ~(str/upper-case (name sym)) %1 %2 %3)
                       {})))))
