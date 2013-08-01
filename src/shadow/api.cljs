(ns shadow.api
  (:require-macros [shadow.macros :refer (wait)])
  (:require [cljs.reader :as reader]
            [shadow.xhr :as xhr]
            [shadow.object :as so]
            [clojure.set :as set]))

(defn ^:export load [queue]
  (doseq [[js-mod fn-id args] queue]
    (let [clj-args (reader/read-string args)
          queued-fn (goog/getObjectByName fn-id)]
      (if queued-fn
        (do (so/log "init" js-mod fn-id)
            (apply queued-fn clj-args))
        (so/log "unknown init function" js-mod fn-id clj-args)))))

(defn ^:export module-ready [module-name]
  (so/log "module-ready" module-name))

(defn ^:export module-error [module-name e]
  (so/log "module-error" module-name e))
