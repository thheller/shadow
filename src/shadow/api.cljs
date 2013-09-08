(ns shadow.api
  (:require-macros [shadow.macros :refer (wait)])
  (:require [cljs.reader :as reader]
            [shadow.xhr :as xhr]
            [shadow.object :as so]
            [shadow.dom :as dom]
            [clojure.set :as set]
            [clojure.string :as str]))

(defn run-init-function [fn-id args]
  (let [queued-fn (goog/getObjectByName fn-id)]
    (if queued-fn
      (do (so/log "init" fn-id)
          (apply queued-fn args))
      (so/log "unknown init function" fn-id args))))

(defn run-script-tags [module-name]
  (doseq [script (dom/query (str "script[type=\"shadow/run\"][data-module=\"" module-name "\"]"))
          :let [init-fn (dom/data script :fn)
                args (dom/get-html script)
                args (when (and args (not= "" args))
                       (reader/read-string args))]]

    (run-init-function init-fn args) 
    (dom/set-data script :success "true")
    ))

(defn ^:export module-ready [module-name]
  (so/log "module-ready" module-name)
  (run-script-tags module-name))

(defn ^:export module-error [module-name e]
  (so/log "module-error" module-name e))
