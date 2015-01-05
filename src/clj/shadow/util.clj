(ns shadow.util)

(defmacro log
  [& args]
  `(let [console# (aget js/window "console")]
     (when console#
       (.log console# ~@(for [arg args]
                          `(shadow.util/console-friendly ~arg))))))

(defmacro with-timing [msg & body]
  `(let [time?# (aget js/window "console" "time")
         msg# ~msg]
     (when time?# 
       (.time js/console msg#))
     ~@body
     (when time?#
       (.timeEnd js/console msg#))
     ))

(defmacro doarray [[binding arr :as bindings] & body]
  (when-not (= 2 (count bindings))
    (throw (ex-info "doarray only supports one binding" {:bindings bindings})))
  `(dotimes [i# (.-length ~arr)]
     (let [~binding (aget ~arr i#)]
       ~@body
       )))
