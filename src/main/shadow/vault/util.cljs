(ns shadow.vault.util
  (:require [shadow.vault.store :as store]))

(defrecord Callback [the-fn args]
  IFn
  (-invoke [this v1]
    (apply the-fn (conj args v1)))
  (-invoke [this v1 v2]
    (apply the-fn (conj args v1 v2)))
  (-invoke [this v1 v2 v3]
    (apply the-fn (conj args v1 v2 v3))))

(defn invoke-tx [tx & args]
  (store/transact! [(apply tx args)]))

(defn tx-callback [tx & args]
  (->Callback invoke-tx (into [tx] args)))

(defn callback
  ([the-fn p1]
   (Callback. the-fn [p1]))
  ([the-fn p1 p2]
   (Callback. the-fn [p1 p2]))
  ([the-fn p1 p2 p3]
   (Callback. the-fn [p1 p2 p3]))
  ([the-fn p1 p2 p3 p4]
   (Callback. the-fn [p1 p2 p3 p4]))
  ([the-fn p1 p2 p3 p4 & more]
   (Callback. the-fn (into [p1 p2 p3 p4] more))))

(extend-protocol IPrintWithWriter
  Callback
  (-pr-writer [this w opts]
    ;; do not want to print the source of the callback function
    (-write w "#shadow.vault/callback [FIXME]")))
