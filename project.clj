(defproject thheller/shadow-client "0.7.0-SNAPSHOT"
  :description "a cljs library for managed state in the shadows of the DOM"
  :url "http://github.com/thheller/shadow"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}

  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.clojure/core.async "0.1.319.0-6b1aca-alpha"]]
  
  :profiles {:dev {:source-paths ["dev"]
                   :dependencies [[org.clojure/clojurescript "0.0-2311" :exclusions [org.mozilla/rhino]]
                                  [thheller/shadow-build "0.9.1"]
                                  [com.cemerick/clojurescript.test "0.2.1"]]}}

  :source-paths ["src/clj" "src/cljs"])
