(ns shadow.api
  (:require-macros [shadow.macros :refer (wait)])
  (:require [cljs.reader :as reader]
            [shadow.xhr :as xhr]
            [shadow.object :as so]
            [clojure.set :as set]))

(def module-index (atom {}))
(def modules-available (atom #{}))

(def init-queue (atom []))

;; this is horrible, atom is not exactly the right tool for the job
(defn run-init-functions! []
  (let [leftovers
        (reduce (fn [leftovers [js-mod fn-id args :as queued]]
                  (if (contains? @modules-available js-mod)
                    (do (let [clj-args (reader/read-string args)
                              queued-fn (goog/getObjectByName fn-id)]
                          (if queued-fn
                            (do (so/log "init" js-mod fn-id)
                                (apply queued-fn clj-args))
                            (so/log "unknown init function" js-mod fn-id clj-args)))
                        leftovers)
                    (conj leftovers queued)))
                []
                @init-queue)]
    (reset! init-queue leftovers)))

(defn get-all-deps [mod-index mod-id]
  (if-let [resolved (get-in mod-index [mod-id :all-deps])]
    ;; was already resolved
    resolved
    ;; resolve deps
    (let [my-deps (set (get-in mod-index [mod-id :depends-on]))]
      (reduce set/union (conj my-deps mod-id) (map #(get-all-deps mod-index %) my-deps))
      )))

(defn resolve-all-deps [mod-index]
  (reduce (fn [state mod-id]
            (let [all-deps (get-all-deps state mod-id)]
              (assoc-in state [mod-id :all-deps] all-deps)))
          mod-index
          (::module-order mod-index)))

(defn lazy-eval! []
  (doseq [mod-id (::module-order @module-index)
          :when (not (contains? @modules-available mod-id))
          :let [source (get-in @module-index [mod-id :source])]
          :while source]

    (so/log "lazy-eval!" mod-id)
    ;; eval'ing should call module-ready 
    (js/eval source)
    ;; dont hang on to source
    (swap! module-index update-in [mod-id] dissoc :source)
    ))

(defn source-loaded! [mod-id source]
  (let [mod-deps (get-in @module-index [mod-id :all-deps])
        mod-deps (disj mod-deps mod-id)]
    (so/log "source-loaded!" mod-id mod-deps @modules-available)
    (if (set/superset? @modules-available mod-deps)
      ;; all deps are available and eval'd, so eval the new one
      (do (js/eval source))
      ;; not all deps were eval'd yet
      (do (swap! module-index assoc-in [mod-id :source] source)
          (lazy-eval!)))))

(defn load-some-modules! []
  (let [required (reduce set/union (map (fn [[mod _ _]]
                                          (get-in @module-index [mod :all-deps]))
                                        @init-queue))]
    (doseq [mod-id (::module-order @module-index)
            :when (and (contains? required mod-id)
                       (not (contains? @modules-available mod-id))
                       (not (get-in @module-index [mod-id :requested]))
                       (not (get-in @module-index [mod-id :source])))]
      ;; fire all request in order
      ;; let the browser handle parallel downloads
      ;; modules may still arrive out of order
      (let [req (xhr/request :GET (get-in @module-index [mod-id :js-path]))]
        (swap! module-index assoc-in [mod-id :requested] true)
        (wait [source req]
          (source-loaded! mod-id source)
          )))))

(defn ^:export load [queue js-modules]
  (let [mods (js->clj js-modules :keywordize-keys true)
        mod-order (vec (map :id mods))
        mod-index (reduce (fn [m {:keys [id] :as mod}]
                            (assoc m id mod))
                          {::module-order mod-order}
                          mods)
        mod-index (resolve-all-deps mod-index)]

    (reset! module-index mod-index)
    (reset! init-queue (js->clj queue))
    
    ;; the common module is already loaded, run those that only need that
    (run-init-functions!)

    (load-some-modules!)
    ))

(defn ^:export module-ready [module-name]
  (swap! modules-available conj module-name)
  (so/log "module-ready" module-name @modules-available)
  (run-init-functions!))

(defn ^:export module-error [module-name e]
  (so/log "module-error" module-name e))
