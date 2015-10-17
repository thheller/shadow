(defproject thheller/shadow-client "1.0.1"
  :description "a cljs library for managed state in the shadows of the DOM"
  :url "http://github.com/thheller/shadow"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}

  :dependencies [[org.clojure/clojure "1.7.0"]
                 [org.clojure/core.async "0.1.346.0-17112a-alpha"]]
  
  :profiles {:dev {:source-paths ["dev"]
                   :dependencies [[thheller/clojurescript "0.0-2665+948"]
                                  [thheller/shadow-build "1.0.0-alpha4"]]}}

  :source-paths ["src/clj"
                 "src/cljs"]
  ) 
