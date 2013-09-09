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
                dom-ref (dom/data script :ref) 
                args (dom/get-html script)
                args (when (and args (not= "" args))
                       (reader/read-string args))
                args (condp = dom-ref
                       "none" args
                       "parent" (cons (dom/get-parent script) args)
                       "previous-sibling" (cons (dom/get-previous-sibling script) args)
                       "next-sibling" (cons (dom/get-next-sibling script) args)
                       (throw (ex-info "script tag with invalid dom ref" {:dom-ref dom-ref :init-fn init-fn :script script})))]]

    (run-init-function init-fn args) 
    ))

(defn ^:export module-ready [module-name]
  (so/log "module-ready" module-name)
  (run-script-tags module-name))

(defn ^:export module-error [module-name e]
  (so/log "module-error" module-name e))
