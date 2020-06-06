(defproject thheller/shadow-client "1.3.3"
  :description "a cljs library for managed state in the shadows of the DOM"
  :url "http://github.com/thheller/shadow"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}

  :repositories
  {"clojars" {:url "https://clojars.org/repo"
              :sign-releases false}}

  :dependencies
  [[org.clojure/clojurescript "1.9.946" :scope "provided"]
   [org.clojure/core.async "0.3.443"]
   [hiccup "1.0.5"]]

  :source-paths
  ["src/main"]

  :test-paths
  ["src/test"])
