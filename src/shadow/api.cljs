(ns shadow.api
  (:require [cljs.reader :as reader]
            [shadow.object :as so]))

(defn ^:export load [queue]
  (doseq [[fn-id args] queue]
    (let [clj-args (reader/read-string args)
          queued-fn (goog/getObjectByName fn-id)]
      (if queued-fn
        (apply queued-fn clj-args)
        (so/log "unknown init function" fn-id clj-args))
      )))
