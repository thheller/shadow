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


