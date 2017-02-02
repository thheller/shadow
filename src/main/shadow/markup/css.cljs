(ns shadow.markup.css
  (:require-macros [shadow.markup.css :as m])
  (:require [shadow.markup.css.impl.gen :as gen]
            [shadow.markup.css.impl.react :as impl]))

(defn set-env! [env]
  (impl/set-env! env))

(defn root [attrs & rules]
  (gen/root* attrs rules))

(defn rule [selector attrs]
  (gen/rule selector attrs))

(defn nested-rule [parent attrs]
  (gen/nested-rule parent attrs))

(defn element*
  "don't use directly, use defstyled macro"
  [el-type el-selector style-fn]
  (impl/->StyledElement el-type el-selector style-fn false))