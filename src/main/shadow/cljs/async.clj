(ns shadow.cljs.async)

(defmacro await [form]
  `(shadow.cljs.async/unwrap-result
     (~'<! (shadow.cljs.async/promise->chan ~form))))

;; convenience so you can do easy stuff without a separate cljs.core.async require

(defmacro go [& body]
  `(cljs.core.async/go ~@body))

(defmacro go-loop [& body]
  `(cljs.core.async/go-loop ~@body))
