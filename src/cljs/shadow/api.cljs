(ns shadow.api
  (:require-macros [shadow.api :as m])
  (:require [cljs.reader :as reader]
            [shadow.dom :as dom]
            [clojure.string :as str]
            [shadow.util :as util :refer (log)]))

(def ready-ref
  (atom {}))

(def load-order-ref
  (atom []))

(defn script->dom-el [script]
  (when-let [dom-ref (dom/data script :ref)]
    (condp = dom-ref
      "none"
      nil

      "self"
      script

      "parent"
      (dom/get-parent script)

      "previous-sibling"
      (dom/get-previous-sibling script)

      "next-sibling"
      (dom/get-next-sibling script)

      (throw (ex-info "script tag with invalid dom ref" {:dom-ref dom-ref :script script})))))

(defn run-script-tag
  "a <script type=\"shadow/run\" data-fn=\"js-fn\">edn-args</script> tag is meant to embed calls to javascript in html
   instead of writing the javascript inline, we only define the call and its args + the location in the dom
   we want to reference. this allows the javascript to be loaded as late as possible, avoids unknown reference errors,
   does not litter the html with $(function() {}); and since a dom reference point is provided it makes it more
   logical to reference dom elements via the server, no need to mess with id/class selectors.

   script tags will be executed as soon as the js module is loaded (assuming it called module-ready), not on dom ready
   which means it triggers earlier"
  [script]
  (let [init-fn
        (dom/data script :fn)

        args
        (dom/get-html script)

        args
        (when (and args (not= "" args))
          (reader/read-string args))

        dom-el
        (script->dom-el script)

        args
        (if dom-el
          (cons dom-el args)
          args)]

    (let [queued-fn (goog/getObjectByName init-fn)]
      (if queued-fn
        (do (log "init" init-fn)
            (apply queued-fn args))
        (log "unknown init function" init-fn args)))))

(defn script-tags-for-ns [ns-name]
  (let [ns-name (str/replace ns-name #"-" "_")]
    (for [script
          (dom/query "script[type=\"shadow/run\"]")

          :let
          [fn (dom/data script :fn)
           fn-ns (.substring fn 0 (.lastIndexOf fn "."))]

          :when
          (= ns-name fn-ns)]

      script
      )))

(defn run-embedded-tags
  "use after calling (dom/set-html node html) and that html may contain embedded script tags

  only runs tags when the namespace of the function is already loaded, if the ns is not yet loaded to ns-ready function
  will pick remaining tags"
  [node]
  (doseq [script
          (dom/query "script[type=\"shadow/run\"]" node)

          :let
          [fn (dom/data script :fn)
           fn-ns (.substring fn 0 (.lastIndexOf fn "."))]

          :when
          (contains? @ready-ref fn-ns)]

    (run-script-tag script)))

(defn run-tags-for-ns [ns-name]
  (log "ns-ready" ns-name)
  (doseq [script (script-tags-for-ns ns-name)]
    (run-script-tag script)))

(defn restart []
  (doseq [ns-name
          @load-order-ref
          :let
          [{:keys [reloadable] :as opts}
           (get @ready-ref ns-name)]

          :when reloadable]
    (run-tags-for-ns ns-name)
    ))

(defn ^:export ns-ready*
  "use (ns-ready) macro, do not use this directly"
  [ns-name opts]
  ;; use setTimeout so the blocking load of a script
  ;; doesn't continue to block while running the init fns
  ;; don't run things twice, not live-reload friendly
  (when-not (contains? @ready-ref ns-name)
    (swap! ready-ref assoc ns-name opts)
    (swap! load-order-ref conj ns-name)
    (js/setTimeout #(run-tags-for-ns ns-name) 0)))
