(ns shadow.cljs.async
  (:require-macros [shadow.cljs.async])
  (:require [cljs.core.async :as async]))

(deftype PromiseResult [success val])

(defn unwrap-result [^clj result]
  (if (.-success result)
    (.-val result)
    (throw (.-val result))
    ))

(defn promise->chan [^js promise]
  (let [ch (async/chan)]
    (-> promise
        (.then
          (fn [result]
            (async/put! ch (PromiseResult. true result))
            (async/close! ch))
          (fn [error]
            (async/put! ch (PromiseResult. false error))
            (async/close! ch))))
    ch))


;; convenience delegates

(defn timeout
  "Returns a channel that will close after msecs"
  [msecs]
  (async/timeout msecs))

(defn close!
  [port]
  (async/close! port))
