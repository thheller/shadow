(ns shadow.xhr
  (:require [shadow.result :as sresult]
            [goog.result :as gresult]
            [goog.labs.net.xhr :as gxhr]
            [cljs.reader :as cr]
            [goog.uri.utils :as gutils]
            [goog.json :as gjson]
            ))

;; FIXME: this whole module is a mess ...

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
     (not= -1 (.indexOf content-type "text/plain"))
     (.-responseText req)
     (not= -1 (.indexOf content-type "javascript"))
     (.-responseText req)
     :else
     (throw (ex-info "unsupported content-type" {:req req :content-type content-type}))
     )))

(defn make-url [url params]
  (gutils/appendParamsFromMap url (clj->js params)))

(defn request
  ([method url]
     (request method url nil {}))
  ([method url data]
     (request method url data {}))
  ([method url data options]
     (when (and (not (contains? #{:GET :DELETE} method)) (nil? data))
       (throw (ex-info "request needs data" {:method method :url url :data data :options options})))

     (let [req (gxhr/send (name method)
                          (if (and (= :GET method) data)
                            (make-url url data)
                            url)
                          (when-not (or (= :GET method) (nil? data))
                            (pr-str data))
                          (make-request-options (assoc-in options [:headers "Content-Type"] "text/edn; charset=utf-8"))
                          )]
       (goog.result/transform req auto-transform))))


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

(defn upload [url file & events]
  (let [xhr (js/XMLHttpRequest.)]
    (doseq [[ev-id ev-handler] (partition 2 events)
            :let [target (if (= :progress ev-id) (aget xhr "upload") xhr)]]
      (.addEventListener target (name ev-id) ev-handler))
    (doto xhr
      (.open "PUT" url)
      (.send file))))
