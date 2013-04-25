(ns shadow.xhr
  (:require [goog.result :as gresult]
            [goog.labs.net.xhr :as gxhr]
            [cljs.reader :as cr]
            [goog.uri.utils :as gutils]
            [goog.json :as gjson]
            ))

(defn- edn-transform [body]
  (cr/read-string body))

(defn- json-transform [body]
  (js->clj (.parse js/JSON body)))

(def goog-keys {:timeout "timeoutMs"
                :with-credentials "withCredentials"
                :headers "headers"
                :mime-type "mimeType"
                })
(defn make-request-options [options]
  (let [obj (js-obj)]
    (doseq [[k v] options]
      (aset obj (get goog-keys k) (clj->js v)))

    (.log js/console "request-options" obj)
    obj
    ))

(defn auto-transform [req]
  (let [content-type (.getResponseHeader req "Content-Type")]
    (cond
     (not= -1 (.indexOf content-type "text/edn"))
     (edn-transform (.-responseText req))
     (not= -1 (.indexOf content-type "json"))
     (json-transform (.-responseText req))
     (not= -1 (.indexOf content-type "text/html"))
     (.-responseText req)
     :else
     (throw (ex-info "unsupported content-type" {:req req :content-type content-type}))
     )))

(defn request [method url data options]
  (let [req (gxhr/send (name method)
                       url
                       (when-not (or (nil? data) (= :GET method))
                         (pr-str data))
                       (make-request-options (assoc-in options [:headers "Content-Type"] "text/edn; charset=utf-8"))
                       )]
    (goog.result/transform req auto-transform)))


(defn get-edn [url]
  (request :GET url nil {}))

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
