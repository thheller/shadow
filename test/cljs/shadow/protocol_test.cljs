(ns shadow.protocol-test
  (:require-macros [shadow.macros :refer (log)])
  (:require [cemerick.cljs.test :as t :refer-macros (is deftest)]
            [shadow.object :as so]
            [shadow.dom :as dom]
            [clojure.string :as str]))

(deftest hello-world-test
  (prn "wtf")
  (.log js/console "hello world")
  (is (= 1 2)))
