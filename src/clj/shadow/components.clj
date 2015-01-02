(ns shadow.components)

(defmacro defc [name & spec]
  (let [spec (apply hash-map spec)
        spec (assoc spec :name (str *ns* "/" name))]
    `(def ~name
       (let [spec# (shadow.components/optimize-spec ~spec)]
         (shadow.components/ElementFactory. #(create-instance spec# %1 %2 %3 %4) {} [])))))


(defmacro $for [bindings body]
  `(let [items# (cljs.core/array)]
     (doseq ~bindings
       (.push items# ~body))
     items#))


