(ns shadow.react.component)

;; wrapping everything in a delay so things can be removed by Closure if not used
(defmacro deffactory [name body]
  (let [type (keyword (str *ns*) (str name))]
    `(let [config#
           (-> ~body
               (delay))

           component#
           (-> (deref config#)
               (assoc :shadow.react.component/type ~type)
               (shadow.react.component/make-component)
               (delay))]

       (defn ~name [props# & children#]
         (shadow.react.component/create-element* (deref component#) props# children#))

       ;; React-interop, some things require access to the component constructor
       ;; (deffactory my-component ...) @my-component
       ;; I prefer this over always declaring two vars one for the component one for the factory
       (cljs.core/specify! ~name
         cljs.core/IDeref
         (~'-deref [x#]
           (cljs.core/-deref component#)))

       (js/goog.object.set ~name "shadow$component" #(deref config#)))))