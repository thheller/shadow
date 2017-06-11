(defproject thheller/shadow-client "1.0.20170607"
  :description "a cljs library for managed state in the shadows of the DOM"
  :url "http://github.com/thheller/shadow"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}

  :dependencies
  [[org.clojure/clojurescript "1.9.542" :scope "provided"]
   [org.clojure/core.async "0.3.442"]
   [hiccup "1.0.5"]]

  :source-paths
  ["src/main"]

  :test-paths
  ["src/test"])
