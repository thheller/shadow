(ns shadow.react.component)

;; wrapping everything in a delay so things can be removed by Closure if not used
(defmacro deffactory
  [name & kv-pairs]
  (assert (even? (count kv-pairs)))
  (let [type
        (keyword (str *ns*) (str name))

        cmp
        (-> (str name "$init")
            (symbol)
            (with-meta {:tag 'not-native}))

        cmp-map
        (-> (apply array-map kv-pairs)
            (assoc ::type type))]

    ;; FIXME: not proper to introduce a new var but takes care of some DCE issues
    `(do (def ~cmp
           (-> ~cmp-map
               (shadow.react.component/make-component)
               (delay)))

         (defn ~name [props# & children#]
           (shadow.react.component/create-element* (cljs.core/-deref ~cmp) props# children#))

         ;; React-interop, some things require access to the component constructor
         ;; (deffactory my-component ...) @my-component or @my-component$init
         (cljs.core/specify! ~name
           cljs.core/IDeref
           (~'-deref [x#]
             (cljs.core/-deref ~cmp)))

         (js/goog.object.set ~name "shadow$component" #(cljs.core/-deref ~cmp)))))