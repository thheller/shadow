(ns shadow.api)

(defmacro ns-ready
  ([]
   `(shadow.api/ns-ready* ~(str *ns*) {}))
  ([opts]
   `(shadow.api/ns-ready* ~(str *ns*) ~opts)))
