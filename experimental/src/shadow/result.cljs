(ns shadow.result
  (:use-macros [shadow.macros :only (js-global)])
  (:require [goog.result :as gresult]))

(defn chain [res handler]
  (gresult/chain res handler))

(defn success? [res]
  (= (js-global goog.result.Result.State.SUCCESS)
     (.getState res)))

(defn value [res]
  (.getValue res))

(defn immediate [value]
  (goog.result/successfulResult value))

(defn error [res callback]
  (gresult/waitOnError res callback))

