(ns shadow.api
  (:require-macros [shadow.macros :refer (wait log)])
  (:require [cljs.reader :as reader]
            [shadow.xhr :as xhr]
            [shadow.object :as so]
            [shadow.dom :as dom]
            [clojure.set :as set]
            [clojure.string :as str]))

(defn run-script-tag
  "a <script type=\"shadow/run\" data-module=\"mod-name\" data-fn=\"js-fn\">args</script> tag is meant to embed calls to javascript in html
   instead of writing the javascript inline, we only define the call and its args + the location in the dom
   we want to reference. this allows the javascript to be loaded as late as possible, avoids unknown reference errors,
   does not litter the html with $(function() {}); and since a dom reference point is provided it makes it more
   logical to reference dom elements via the server, no need to mess with id/class selectors.

   script tags will be executed as soon as the js module is loaded (assuming it called module-ready), not on dom ready
   which means it triggers earlier"
  [script]
  (let [init-fn (dom/data script :fn)
        dom-ref (dom/data script :ref) 
        args (dom/get-html script)
        args (when (and args (not= "" args))
               (reader/read-string args))
        args (condp = dom-ref
               nil args
               "none" args
               "self" (cons script args)
               "parent" (cons (dom/get-parent script) args)
               "previous-sibling" (cons (dom/get-previous-sibling script) args)
               "next-sibling" (cons (dom/get-next-sibling script) args)
               (throw (ex-info "script tag with invalid dom ref" {:dom-ref dom-ref :init-fn init-fn :script script})))]

    (let [queued-fn (goog/getObjectByName init-fn)]
    (if queued-fn
      (do (log "init" init-fn)
          (apply queued-fn args))
      (log "unknown init function" init-fn args)))))

(def modules (atom #{}))

(defn run-embedded-tags
  "use after calling (dom/set-html node html) and that html may contain embedded script tags

  only runs tags where the module is already loaded, if the module is not yet loaded to module-ready function
  will pick remaining tags"
  [node]
  (doseq [script (dom/query "script[type=\"shadow/run\"]" node)]
    (let [module (dom/data script :module)]
      (when (contains? @modules module)
        (run-script-tag script)))))

(defn ^:export module-ready [module-name]
  (log "module-ready" module-name)
  (swap! modules conj module-name)

  ;; module finished loading, run all associated script tags
  ;; eg. <script type="shadow/run" data-module="something" data-fn="my_ns.some_fn">args data (pr-str args) on server</script>
  (comment
    (doseq [script (dom/query (str "script[type=\"shadow/run\"][data-module=\"" module-name "\"]"))]
      (run-script-tag script)
      )))

(defn ^:export ns-ready [ns-name]
  (log "ns-ready" ns-name)
  (let [ns-name (str/replace ns-name #"-" "_")]
    (doseq [script (dom/query "script[type=\"shadow/run\"]")
            :let [fn (dom/data script :fn)
                  fn-ns (.substring fn 0 (.lastIndexOf fn "."))]
            :when (= ns-name fn-ns)]
      (run-script-tag script))))
