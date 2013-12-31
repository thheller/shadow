(defproject thheller/shadow-client "0.1.1"
  :description "a cljs library for managed state in the shadows of the DOM"
  :url "http://github.com/thheller/shadow"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}

  :dependencies [[org.clojure/clojure "1.5.1"]
                 [org.clojure/clojurescript "0.0-2138"]
                 [org.clojure/core.async "0.1.267.0-0d7780-alpha"]]
  
  :profiles {:dev {:source-paths ["dev"]
                   :dependencies [[org.clojure/clojurescript "0.0-2127" :exclusions [org.mozilla/rhino]]
                                  [thheller/shadow-build "0.1.0"]
                                  [com.cemerick/clojurescript.test "0.2.1"]]}}

  :source-paths ["src/clj" "src/cljs"])
