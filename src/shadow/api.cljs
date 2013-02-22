(ns shadow.api)

(defn ^:export load [queue]
  (doseq [[queued-fn & args] queue]
    (let [clj-args (js->clj args :keywordize-keys true)
          queued-fn (goog/getObjectByName queued-fn)]
      (apply queued-fn clj-args)
      )))
