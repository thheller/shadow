(ns shadow.vault.dom
  (:require [shadow.react.component :as comp]
            [shadow.vault.context :as ctx]
            [cljsjs.react.dom]))

;; FIXME: figure out if React already has the concept of ::depth built-in
;; we want to track the depth of each mounted component
;; so we can render top-down
;; this potentially saves child-renders of the top decides to remove them
(defn mount
  ([dom-target react-el]
   (mount dom-target react-el {}))
  ([dom-target react-el root-context]
   (js/ReactDOM.render
     (ctx/shadow-context
       {:context (assoc root-context ::ctx/depth 0)
        :root react-el})
     dom-target)
    ))

(defn unmount [el]
  (js/ReactDOM.unmountComponentAtNode el))
