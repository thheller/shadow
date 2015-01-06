(ns shadow.dev-tools
  "not really dev tools, but could be at some point. just for debugging stuff right now"
  (:require [shadow.components :as sc :refer (defc $)]
            [shadow.dom :as dom]
            [shadow.html :as html]))

(defc scope-view
  :dom (fn [{:keys [scope] :as this}]
         (let [{:keys [id name owner children]} scope]
           ($ (html/div {:class "scope"})
              ($ html/b (if owner (-> owner (.-spec) :name) name))
              ($ (html/div {:class "scope-children"})
                 (for [child children]
                   (scope-view {:scope child})))))))

(defc root-view
  :dom (fn [{:keys [scope] :as this}]
         ($ (html/div {:class "scope-snapshot container"}) 
            (scope-view {:scope scope}))
         ))

(defn make-snapshot [scope]
  {:id (.-id scope)
   :name (.-name scope)
   :owner (.-owner scope)
   :children (mapv make-snapshot @(.-children scope))})

(def last-snapshot (atom nil))

(defn scope-snapshot []
  (when-let [s @last-snapshot]
    (sc/destroy! s)
    (reset! last-snapshot nil))

  (let [el (sc/construct (root-view {:scope (make-snapshot sc/root-scope)}))]
    (reset! last-snapshot el)
    (dom/append el)))

