(defproject thheller/shadow-client "1.0.161"
  :description "a cljs library for managed state in the shadows of the DOM"
  :url "http://github.com/thheller/shadow"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}

  :dependencies [[org.clojure/clojure "1.7.0"]
                 [org.clojure/core.async "0.2.371"]]
  
  :profiles {:dev {:source-paths ["dev"]
                   :dependencies [[org.clojure/clojure "1.7.0"]
                                  [thheller/shadow-devtools "0.1.34"]]}}

  :source-paths ["src/clj"
                 "src/cljs"]
  ) 
