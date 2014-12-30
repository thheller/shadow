(defproject thheller/shadow-client "0.7.0-SNAPSHOT"
  :description "a cljs library for managed state in the shadows of the DOM"
  :url "http://github.com/thheller/shadow"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}

  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.clojure/core.async "0.1.346.0-17112a-alpha"]]
  
  :profiles {:dev {:source-paths ["dev"]
                   :dependencies [[org.clojure/clojurescript "0.0-2511"]
                                  [thheller/shadow-build "1.0.0-alpha3"]]}}

  :source-paths ["src/clj"]
  :resource-paths ["src/cljs"]

  :aliases {"cljs-dev" ["run" "-m" "shadow.cljs.api/build-dev" :project/cljs]
            "cljs-prod" ["run" "-m" "shadow.cljs.api/build-prod" :project/cljs]}
  
  :cljs {:modules [{:name :test
                    :main 'shadow.test-app}]
         
         :live-reload {:before-load 'shadow.test-app/stop}

         :source-paths ["src/cljs"
                        "src/test-app"]
         :public-dir "target/cljs-out"
         :public-path "target/cljs-out"}
  )
