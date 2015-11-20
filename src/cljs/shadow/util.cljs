(ns shadow.util
  (:require-macros [shadow.util :as m])
  (:require [cljs.core.async :as async])) ;; not really used here, but the go! macro needs this

(defn console-friendly [a]
  (cond
   (nil? a) "nil"
   (keyword? a) (str a)
   (string? a) a
   (number? a) a
   (satisfies? IPrintWithWriter a) (pr-str a)
   :else a
   ))

(defn go! [& body]
  (throw (ex-info "go! is a macro" {})))

(def console? (exists? js/console))

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
(comment
  (def default-ex-info js/cljs.core.ex-info)

  (set! (.. js/cljs -core -ex-info)
    (fn shadow-ex-info
      ([msg map]
       (m/log "EX-INFO:" msg map)
       (default-ex-info msg map))
      ([msg map cause]
       (m/log "EX-INFO:" msg map cause)
       (default-ex-info msg map cause))
      )))


(defn remove-from-vector [coll key]
  (->> (map-indexed vector coll)
       (reduce (fn [v [idx item]]
                 (if (= idx key)
                   v
                   (conj! v item)))
               (transient []))
       (persistent!)))

(defn remove-item-from-coll [coll key value]
  (cond
   (satisfies? IVector coll)
   (remove-from-vector coll key)
   (satisfies? IMap coll)
   (dissoc coll key)
   (satisfies? ISet coll)
   (disj coll value)
   :else (throw (ex-info "unknown coll type" {:coll coll :key key :value value}))
   ))
