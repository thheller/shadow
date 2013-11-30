(ns shadow.animate
  (:require-macros [cljs.core.async.macros :refer (go)])
  (:require [shadow.dom :as dom]
            [clojure.string :as str]
            [cljs.core.async :refer (timeout)]))

(defprotocol TransitionDefinition
  (-transition-attr [this])
  (-transition-from [this])
  (-transition-to [this])
  (-transition-toggle? [this] "should return true if the attribute is not to be transitioned")
  (-transition-timing [this] "should return nil if its just a toggle"))

(extend-protocol TransitionDefinition
  PersistentVector
  (-transition-attr [this] (nth this 0))
  (-transition-from [this] (nth this 1))
  (-transition-to [this] (nth this 2))
  (-transition-toggle? [this] (= 3 (count this)))
  (-transition-timing [this] (nth this 3)))

(defn- transition-string [duration attrs]
  (->> attrs
       (map (fn [tdef]
              (str (name (-transition-attr tdef)) " " (-transition-timing tdef) " " duration "ms")))
       (str/join ", ")))

(defn- transition-values [state-fn defs]
  (reduce (fn [styles tdef]
            (assoc styles (-transition-attr tdef) (state-fn tdef)))
          {}
          defs))

(defn transition
  "define a transition of elements, can be used to combine transitions on multiple elements
   returns a channel you can <! to wait till its finished

   transitions are defined via TransitionDefinition protocol, vectors by default
   eg. simple vectors [attr-to-transition from to timing-function]
                     [attr-to-toggle from to] ;; this will toggle the attr from to after the duration

   (transition 500 {some-el [[:opacity \"0\" \"1\" \"ease-in\"]]
                    other-el [[:display \"block\" \"none\"]})

   when vectors are too confusing, use the helper functions
   eg.
   (transition-attr :opacity \"0\" \"1\" \"ease-in\") = [:opacity \"0\" \"1\" \"ease-in\"]
   (toggle-attr :display \"block\" \"none\")

   transition some-el opacity from 0 to 1 using ease-in and 500ms
   toggle other-el display to none after 500ms"
  [duration elements]
  (let [items (for [[el attrs] elements]
                (let [toggles (filter -transition-toggle? attrs)
                      transitions (remove -transition-toggle? attrs)]
                  {:el el
                   :from (transition-values -transition-from attrs)
                   :transition (transition-string duration transitions)
                   :to (transition-values -transition-to transitions)
                   :toggle (transition-values -transition-to toggles)}))]
    (go (doseq [{:keys [el from]} items]
          (dom/set-style el from))
        (<! (timeout 0)) ;; give dom a chance to apply styles
        (doseq [{:keys [el to transition]} items]
          (dom/set-style el (assoc to
                              :transition transition)))
        (<! (timeout duration))
        (doseq [{:keys [el transition toggle]} items]
          (dom/set-style el (assoc toggle :transition nil)))
        :done
        )))

(defn transition-attr [attr from to timing]
  (reify TransitionDefinition
    (-transition-attr [_] attr)
    (-transition-from [_] from)
    (-transition-to [_] to)
    (-transition-toggle? [_] false)
    (-transition-timing [_] timing)
    ))

(defn toggle-attr [attr from to]
  (reify TransitionDefinition
    (-transition-attr [_] attr)
    (-transition-to [_] to)
    (-transition-from [_] from)
    (-transition-toggle? [_] true)
    (-transition-timing [_] (throw (ex-info "should not be called" {})))))

(defn fade-in
  ([] (fade-in "ease"))
  ([timing-function]
     (transition-attr :opacity "0" "1" timing-function)
     ))


(comment
  "combined transitions, will unblock when all completed"
  (go (<! (transition 500 {some-el [(fade-in)]
                           other-el [[:display "none" "block"]]
                           more-el [[:height "0px" "100px" "ease"]]}))))
