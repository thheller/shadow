(ns shadow.material.toast
  {:doc "material design inspired toast"
   :url "https://www.polymer-project.org/docs/elements/paper-elements.html#paper-toast"}
  (:require-macros [cljs.core.async.macros :refer (go alt!)])
  (:require [cljs.core.async :as async]
            [shadow.components :as sc :refer (defc $)]
            [shadow.animate :as anim]
            [shadow.html :as html]
            [shadow.dom :as dom]
            [shadow.util :as util :refer (log)]))

(def default-attrs {:class "sm-toast"})

(defc component
  :dom (fn [{:keys [attrs] :as this} children]
         ($ html/div (merge default-attrs
                            {:on [:click (fn [_ _]
                                           (sc/destroy! this))]}
                            attrs)
            
            children)))

;; FIXME: currently toasts can overlap each other, should instead queue after each other
;; FIXME: should probably also animate out
;; FIXME: more config options
(defn display
  [parent
   {:keys [timeout]
    :or {timeout 3000}
    :as attrs}
   & body]
  (let [toast (sc/construct parent ($ component {:attrs attrs} body))
        slide-in (anim/setup 80 {toast (anim/combine
                                        (anim/translate-y "100%" "0" "ease-in-out")
                                        (anim/fade-in "ease-in-out"))})]

    (anim/init! slide-in)
    (dom/append toast) 
    (anim/continue! slide-in)

    (let [timeout (async/timeout timeout)]
      (go (alt!
            toast
            ([_] :toast-died)
            ;; if toast is still alive, we remove it
            timeout
            ([_] (sc/destroy! toast))
            )))))
