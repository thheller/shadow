(defproject perf-test "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.4.0"]]

  :plugins [[lein-cljsbuild "0.3.0"]]

  :source-paths ["src"
                 "../../lib/clojurescript/src/clj"
                 "../../lib/clojurescript/src/cljs"]

  :cljsbuild
  {:builds {:default
            {:source-paths ["src"
                            "../../src"]
             :compiler {:output-to "html/perf.js"
                        :pretty-print true
                        :verbose true}}
            }})
