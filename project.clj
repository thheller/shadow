(defproject thheller/shadow-client "1.0.175"
  :description "a cljs library for managed state in the shadows of the DOM"
  :url "http://github.com/thheller/shadow"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}

  :dependencies [[org.clojure/core.async "0.2.395"]]

  :profiles {:dev
             {:source-paths ["dev"]
              :dependencies [[org.clojure/clojure "1.9.0-alpha14"]
                             [thheller/shadow-build "1.0.239"]
                             [org.clojure/clojurescript "1.9.293"]]}}

  :source-paths ["src/clj"
                 "src/cljs"]
  ) 
