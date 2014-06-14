(ns shadow.xhr
  "FIXME: rewrite to using promises, since closure is deprecating result"
  (:require-macros [shadow.macros :refer (log)])
  (:require [goog.result :as gresult]
            goog.result.SimpleResult
            [goog.labs.net.xhr :as gxhr]
            [cljs.reader :as cr]
            [goog.uri.utils :as gutils]
            [goog.json :as gjson]
            [shadow.object :as so]
            [shadow.dom :as dom]
            [cljs.core.async :as async]
            ))

(defn result-chain [res handler]
  (gresult/chain res handler))

(defn result-combine [& results]
  (apply gresult/combine results))

(defn result-wait-on-success [res callback]
  (gresult/waitOnSuccess res callback))

(defn result-success? [res]
  (= js/goog.result.Result.State.SUCCESS (.getState res)))

(defn result-value [res]
  (.getValue res))

(defn result-immediate [value]
  (gresult/successfulResult value))

(defn result-error [res callback]
  (gresult/waitOnError res callback))

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


(defn transform-request-body [data]
  (cond
   (string? data)
   ["text/plain; charset=utf-8" data]
   (satisfies? IPrintWithWriter data)
   ["text/edn; charset=utf-8" (pr-str data)]
   :default
   ["application/octet-stream" data]
   ))


(defn as-url [input]
  (cond
   (string? input)
   input

   (and (vector? input)
        (= 2 (count input))
        (string? (first input))
        (map? (second input)))
   (let [[url params] input]
     (make-url url params))

   :else
   (throw (ex-info "doesnt look like an url" {:input input}))
   ))

(defn e->data [e]
  (log "transform event to data" e)
  e)

(defn event-handler [req events event-type]
  (fn [e]
    (cond
     (map? events)
     (when-let [c (get events event-type)]
       (async/put! c [req (e->data e)]))

     (not (nil? events))
     (async/put! events [event-type req (e->data e)])

     :else
     nil)))

(defn request-ok? [status]
  (= 200 status))

(defn request-error? [status]
  (>= status 500))

(defn chan
  "creates a channel and performs xhr request like (chan :GET \"/something\")

   accepts url as string or [url params]

   this channel will receive one message [status body req] on load
   loadend will close the result channel, so you will get nil results on request errors/timeouts

   use options hash to pass in extra channels

   (chan :POST \"/something\" some-data {:events chan-or-map
                                         :upload chan-or-map})

   when given a chan that channels receives messages in form of [event-type event-data req]
   when given a map a channel is expected for each eventtype and will receive [event-data req]
   {:timeout some-chan}, if no channel for a given event is present, no message is sent

   event types are :loadstart :progress :abort :error :timeout :loadend :load (see xhr spec)
   only exception is that status >= 500 does not return a result and produces an error event instead"

  ([method url]
     (chan method url nil {}))
  ([method url data]
     (chan method url data {}))
  ([method url data {:keys [events
                            upload]
                     :as options}]
     (let [req (js/XMLHttpRequest.)
           body? (not= :GET method)
           [content-type body] (if body?
                                 (transform-request-body data)
                                 [nil nil])
           result-chan (async/chan 1)]
       
       ;; FIXME: bad for CORS! but who uses http auth for anything serious?
       (set! (.-withCredentials req) true)
       (set! (.-responseType req) "text")

       (when (and body? upload)
         (dom/on (.-upload req) :loadstart (event-handler req upload :loadstart))
         (dom/on (.-upload req) :progress (event-handler req upload :progress))
         (dom/on (.-upload req) :abort (event-handler req upload :abort))
         (dom/on (.-upload req) :error (event-handler req upload :error))
         (dom/on (.-upload req) :timeout (event-handler req upload :timeout))
         (dom/on (.-upload req) :loadend (event-handler req upload :loadend))
         (dom/on (.-upload req) :load (event-handler req upload :loadend)))
           
       (dom/on req :loadstart (event-handler req events :loadstart))
       (dom/on req :progress (event-handler req events :progress))
       (dom/on req :abort (event-handler req event :abort))
       (dom/on req :timeout (event-handler req event :timeout))

       (dom/on req :loadend (let [callback (event-handler req event :loadend)]
                              (fn [e]
                                (callback e)
                                (async/close! result-chan))))

       (let [error-handler (event-handler req event :error)]
         (dom/on req :error error-handler)

         (dom/on req :load (let [callback (event-handler req event :load)]
                             (fn [e]
                               (let [status (.-status req)
                                     body (auto-transform req)]
                                 (if (request-error? status)
                                   (error-handler e)
                                   (do (callback e)
                                       (async/put! result-chan [status body req]))))))))

       (.open req (name method) (as-url url) true)

       (when body?
         (.setRequestHeader req "Content-Type" content-type))

       (if body?
         (.send req body)
         (.send req))

       result-chan
       )))

(defn request
  ([method url]
     (request method url nil {}))
  ([method url data]
     (request method url data {}))
  ([method url data options]
     (when (and (not (contains? #{:GET :DELETE} method)) (nil? data))
       (throw (ex-info "request needs data" {:method method :url url :data data :options options})))

     (let [body? (not (or (= :GET method) (nil? data)))
           [content-type body] (transform-request-body data)
           req (gxhr/send (name method)
                          (if (and (= :GET method) data)
                            (make-url url data)
                            url)
                          (when body?
                            body)
                          (make-request-options
                           (if body?
                             (assoc-in options [:headers "Content-Type"] content-type)
                             options))
                          )
           ;; req (js/goog.result.SimpleResult.fromPromise req)
           ]
       (gresult/transform req auto-transform))))


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
        (gresult/transform edn-transform)
        )))

(defn upload [url file & events]
  (let [xhr (js/XMLHttpRequest.)]
    (doseq [[ev-id ev-handler] (partition 2 events)
            :let [target (if (= :progress ev-id) (aget xhr "upload") xhr)]]
      (.addEventListener target (name ev-id) ev-handler))
    (doto xhr
      (.open "PUT" url)
      (.send file))))
