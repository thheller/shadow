(defproject thheller/shadow-client "1.0.0-SNAPSHOT"
  :description "a cljs library for managed state in the shadows of the DOM"
  :url "http://github.com/thheller/shadow"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}

  :dependencies [[org.clojure/core.async "0.1.346.0-17112a-alpha"]]
  
  :profiles {:dev {:source-paths ["dev"]
                   :dependencies [[org.clojure/clojure "1.7.0"]
                                  [thheller/shadow-devtools "0.1.0"]]}}

  :source-paths ["src/clj"]
  :resource-paths ["src/cljs"]
  ) 
