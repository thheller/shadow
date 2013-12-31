(ns build
  (:require [shadow.cljs.build :as cljs]
            [clojure.java.io :as io]
            [clojure.java.shell :refer (sh)]))


(defn phantom-tests [state]
  (println "======= TESTS ==============================================")
  (let [{:keys [out err] :as results} (sh "phantomjs" "test/runner.js")]
    (println out)
    (when (seq err)
      (println err)))
  (println "======= /TESTS =============================================")
  state)

(defn tests
  "build the project, wait for file changes, run tests, repeat"
  [& args]
  (let [state (-> (cljs/init-state)
                  (cljs/enable-source-maps)
                  (assoc :optimizations :none
                         :pretty-print true
                         :work-dir (io/file "target/cljs-work")
                         :public-dir (io/file "public/cljs")
                         :public-path "cljs")
                  (cljs/step-find-resources-in-jars)
                  (cljs/step-find-resources "src/cljs")
                  (cljs/step-find-resources "test/cljs")
                  (cljs/step-finalize-config)
                  (cljs/step-compile-core)
                  (cljs/step-configure-module :tests ['shadow.protocol-test
                                                      'shadow.test-helper] #{})
                  )]
    
    (loop [state state]
      (recur (-> state
                 (cljs/step-compile-modules)
                 (cljs/flush-unoptimized)
                 (phantom-tests)
                 (cljs/wait-and-reload!)
                 ))))
  :done)
