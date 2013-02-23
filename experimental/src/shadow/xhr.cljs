(ns shadow.xhr
  (:require [goog.result :as gresult]
            [goog.labs.net.xhr :as gxhr]
            [cljs.reader :as cr]
            [goog.uri.utils :as gutils]
            ))

(defn- edn-transform [body]
  (cr/read-string body))

(defn xhr-get [{:keys [url params] :as req}]
  (gxhr/get (gutils/appendParamsFromMap url (clj->js params))))

(defn get-edn [req]
  (-> (xhr-get (if (map? req) req {:url req}))
      (goog.result/transform edn-transform)
      ))

(defn xhr-post [{:keys [url params] :as req} data]
  (gxhr/post (gutils/appendParamsFromMap url (clj->js params))
             data
             (clj->js req)))

(defn post-edn [req data]
  (let [req (if (map? req) req {:url req})
        req (assoc-in req [:headers "Content-Type"] "text-edn")]
    (-> (xhr-post req (pr-str data))
        (goog.result/transform edn-transform)
        )))
