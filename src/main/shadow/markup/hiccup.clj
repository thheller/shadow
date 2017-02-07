(ns shadow.markup.hiccup
  (:require [shadow.markup.css :as css]))

(defmacro defstyled [& args]
  `(css/defstyled ~@args))

;; FIXME: add dom elements