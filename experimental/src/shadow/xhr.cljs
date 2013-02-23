(ns shadow.xhr
  (:refer-clojure :exclude [get])
  (:require [goog.result :as gresult]
            [goog.labs.net.xhr :as gxhr]
            [cljs.reader :as cr]
            [goog.uri.utils :as gutils]
            ))

(defn- edn-transform [body]
  (cr/read-string body))

(defn- do-get [{:keys [url params] :as req}]
 (-> (gxhr/get (gutils/appendParamsFromMap url (clj->js params)))
      (goog.result/transform edn-transform)))

(defn get [req]
  (if-not (map? req)
    (do-get {:url req})
    (do-get req)))
