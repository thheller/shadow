(ns shadow.vault.context
  (:require [shadow.react.component :as comp]))

(def ShadowContext
  {::comp/type
   ::context

   ::comp/extend
   #js {:getChildContext
        (fn []
          (this-as this
            (let [ctx
                  (-> this (.-props) (comp/get-shadow-props) :context)]
              #js {:shadow$context ctx})))}

   ::comp/render
   (fn [this]
     (get-in this [:props :root]))})

(def shadow-context
  (-> ShadowContext
      (comp/factory)))

