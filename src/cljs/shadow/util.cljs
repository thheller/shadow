(ns shadow.util
  {:load-macros true}
  (:require-macros [shadow.util :as macros])
  (:require [shadow.api :as api]))

(defn console-friendly [a]
  (cond
   (nil? a) "nil"
   (keyword? a) (str a)
   (string? a) a
   (number? a) a
   (satisfies? IPrintWithWriter a) (pr-str a)
   :else a
   ))

(def console? (not (nil? (aget js/window "console"))))

(defn log
  ([a1]
     (when console?
       (.log js/console
             (console-friendly a1))))
  ([a1 a2]
     (when console?
       (.log js/console
             (console-friendly a1)
             (console-friendly a2))))
  ([a1 a2 a3]
     (when console?
       (.log js/console
             (console-friendly a1)
             (console-friendly a2)
             (console-friendly a3))))
  ([a1 a2 a3 a4]
     (when console?
       (.log js/console
             (console-friendly a1)
             (console-friendly a2)
             (console-friendly a3)
             (console-friendly a4))))
  ([a1 a2 a3 a4 a5]
     (when console?
       (.log js/console
             (console-friendly a1)
             (console-friendly a2)
             (console-friendly a3)
             (console-friendly a4)
             (console-friendly a5))))
  ([a1 a2 a3 a4 a5 a6]
     (when console?
       (.log js/console
             (console-friendly a1)
             (console-friendly a2)
             (console-friendly a3)
             (console-friendly a4)
             (console-friendly a5)
             (console-friendly a6))))
  ([a1 a2 a3 a4 a5 a6 & more]
     (when console?
       (.log js/console
             (console-friendly a1)
             (console-friendly a2)
             (console-friendly a3)
             (console-friendly a4)
             (console-friendly a5)
             (console-friendly a6)
             "more:"
             (pr-str more)))))

;; HAX

(def default-ex-info js/cljs.core.ex-info)

(set! (.. js/cljs -core -ex-info)
      (fn shadow-ex-info
        ([msg map]
           (macros/log "EX-INFO:" msg map)
           (default-ex-info msg map))
        ([msg map cause]
           (macros/log "EX-INFO:" msg map cause)
           (default-ex-info msg map cause))
        ))

(set! (.. js/goog -asserts -doAssertFailure_)
      (fn [default-message default-args given-message given-args]
        (throw (ex-info "ASSERT FAILED" {:dm default-message :da default-args :gm given-message :ga given-args}))))


