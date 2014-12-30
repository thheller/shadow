(ns shadow.components)

(defmacro defc [name & spec]
  (let [spec (apply hash-map spec)
        spec (assoc spec :name (str *ns* "/" name))]
    `(def ~name
       (let [spec# (shadow.components/optimize-spec ~spec)]
         (shadow.components/ElementFactory. (partial create-instance spec#) {} [])))))


