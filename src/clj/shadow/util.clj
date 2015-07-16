(ns shadow.util
  (:require [cljs.core.async.impl.ioc-macros :as ioc]))

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

(defmacro go!
  "just like go, just executes immediately until the first parkable point"
  [& body]
  `(let [c# (cljs.core.async/chan 1)
         f# ~(ioc/state-machine body 1 &env ioc/async-custom-terminators)
         state# (-> (f#)
                    (ioc/aset-all! cljs.core.async.impl.ioc-helpers/USER-START-IDX c#))]
     (cljs.core.async.impl.ioc-helpers/run-state-machine state#)
     c#))
