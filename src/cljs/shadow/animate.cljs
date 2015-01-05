(ns shadow.animate
  (:require-macros [cljs.core.async.macros :refer (go)])
  (:require [shadow.dom :as dom]
            [shadow.object :as so]
            [clojure.string :as str]
            [cljs.core.async :as async]
            [goog.dom.vendor :as vendor]
            [goog.style :as gs]
            [shadow.util :as util :refer (doarray log)]))

;; FIXME: this needs a cleanup, due to introduction of Animator the whole
;; other stuff seems unnecessary complex and confusing

;; not actually sure a protocol is any help here, just a map of maps would work too
;; started with a different approach, dunno if there are any drawbacks to keeping this
(defprotocol Animation
  (-animate-from [this] "return a map of {attr initial-value}")
  (-animate-to [this] "return a map of {attr target-value}")
  (-animate-toggles [this] "return a map of {attr target-value}")
  (-animate-timings [this] "return a map of {attr timing-function}")
  (-animate-delays [this] "return a map of {attr transition-delay}"))

(defn- transition-string [duration adef]
  (let [timings (-animate-timings adef)
        delays (-animate-delays adef)]
    (->> timings
         (map (fn [[attr timing]]
                (str (name attr)
                     " "
                     duration "ms"
                     " "
                     timing 
                     (let [delay (get delays attr)]
                       (when (and delay (pos? delay))
                         (str " " delay "ms"))))))
         (str/join ", "))))

(defprotocol IAnimator
  (get-duration [animator])
  (init! [animator] "apply the initial values")
  (start! [animator] "start the animation, must return a channel that closes once the animation is done")
  (finish! [animator] "cleanup"))

(deftype Animator [duration items]
  IAnimator
  (get-duration [_] duration)
  (init! [_]
    ;; set from values on all nodes
    (doarray [{:keys [el from]} items]
      (gs/setStyle el from)))
  (start! [_]
    ;; set to values
    (doarray [{:keys [el to transition]} items]
      (aset to "transition" transition)
      (gs/setStyle el to)))
  (finish! [_]
    ;; cleanup
    (doarray [{:keys [el toggles]} items]
      (aset toggles "transition" nil)
      (gs/setStyle el toggles))))

(defrecord AnimationStep [el from to toggles transition])

(defn setup [duration elements]
  (let [items (into-array (for [[el adef] elements]
                            (do (when-not (satisfies? Animation adef)
                                  (throw (ex-info "invalid animation" {:el el :animation adef})))
                                (let [from (-animate-from adef)
                                      to (-animate-to adef)
                                      toggles (-animate-toggles adef)]
                                  (AnimationStep.
                                   (dom/dom-node el)
                                   (clj->js from) 
                                   (clj->js to)
                                   (clj->js toggles)
                                   (transition-string duration adef))))))]
    (Animator. duration items)))

(defn continue! [animator]
  (go (start! animator)
      (<! (async/timeout (get-duration animator)))
      (finish! animator)
      :done))

(defn start [duration elements]
  (let [animator (setup duration elements)]
    (init! animator) 
    (continue! animator)
    ))

;; transitions

(defn transition
  "transition the given attr from -> to using timing function and delay
   timing defaults to ease, delay to 0"
  ([attr from to]
     (transition attr from to "ease" 0))
  ([attr from to timing]
     (transition attr from to timing 0))
  ([attr from to timing delay]
     (reify Animation
       (-animate-from [_] {attr from})
       (-animate-to [_] {attr to})
       (-animate-toggles [_] {})
       (-animate-timings [_] {attr timing})
       (-animate-delays [_] {attr delay})
       )))

(defn toggle [attr from to]
  (reify Animation
    (-animate-to [_] {})
    (-animate-from [_] {attr from})
    (-animate-toggles [_] {attr to})
    (-animate-timings [_] {})
    (-animate-delays [_] {})))

(defn set-attr
  "set attr to value when the animation starts"
  [attr value]
  (reify Animation
    (-animate-to [_] {})
    (-animate-from [_] {attr value})
    (-animate-toggles [_] {})
    (-animate-timings [_] {})
    (-animate-delays [_] {})))

(defn delete-attr
  "use to remove a given attribute style when the animation is finished
   usually only needed to remove attributes we no longer need since they are probably
   inherited and we only used for previous transitions"
  [attr]
  (reify Animation
    (-animate-to [_] {})
    (-animate-from [_] {})
    (-animate-toggles [_] {attr nil})
    (-animate-timings [_] {})
    (-animate-delays [_] {})))

(defn combine [& transitions]
  (loop [to {}
         from {}
         toggles {}
         timings {}
         delays {}
         transitions transitions]
    (if-let [adef (first transitions)]
      ;; TODO: should check for conflicts and throw!
      ;; can't combine transitions on the same attribute
      (recur (merge to (-animate-to adef))
             (merge from (-animate-from adef))
             (merge toggles (-animate-toggles adef))
             (merge timings (-animate-timings adef))
             (merge delays (-animate-delays adef))
             (rest transitions))
      ;; return combined transition
      (reify Animation
        (-animate-from [_] from)
        (-animate-to [_] to)
        (-animate-toggles [_] toggles)
        (-animate-timings [_] timings)
        (-animate-delays [_] delays)))))

;; common transitions
(defn fade-in
  ([] (fade-in "ease-in"))
  ([timing-function]
     (transition :opacity "0" "1" timing-function)
     ))

(defn fade-out
  ([] (fade-in "ease-out"))
  ([timing-function]
     (transition :opacity "1" "0" timing-function)
     ))

(def vendor-prefix (vendor/getVendorPrefix))
;; the transition part for transform is still vendor prefixed! css3 ...
(def vendor-transform (keyword (str vendor-prefix "-transform")))

(defn translate-y
  ([from to timing]
     (translate-y from to timing 0))
  ([from to timing delay]
     (reify Animation
       (-animate-from [_] {:transform (str "translateY(" from ")")})
       (-animate-to [_] {:transform (str "translateY(" to ")")})
       (-animate-timings [_] {vendor-transform timing})
       (-animate-toggles [_] {})
       (-animate-delays [_] {vendor-transform delay}))))
