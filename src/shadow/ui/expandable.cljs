(ns shadow.ui.expandable
  (:require [shadow.object :as obj]
            [shadow.dom :as dom]))


(obj/define-event :expandable-opening "duh?" [])
(obj/define-event :expandable-opened "duh?" [])
(obj/define-event :expandable-closing "duh?" [])
(obj/define-event :expandable-closed "duh?" [])

(comment
  (obj/define-property :expandable-attr "which obj attribute should be used to toggle the expandable (should be a boolen) example :expandable-attr [:showing]")
  (obj/define-property :expandable-dom "function which creates the expandable dom")
  (obj/define-property :expandable-events "dom events which will be attached to the dom created by :expandable-dom"))

(defn- get-state-attr [obj]
  (obj/get-type-attr obj :expandable-attr))

(defn open? [obj]
  (let [state-attr (get-state-attr obj)]
    (get obj state-attr)))

(defn open [obj]
  (when-not (open? obj)
    (obj/notify! obj :expandable-opening)
    (obj/update! obj assoc (get-state-attr obj) true)
    (obj/notify! obj :expandable-opened)))

(defn close [obj]
  (when (open? obj)
    (obj/notify! obj :expandable-closing)
    (obj/update! obj assoc (get-state-attr obj) false)
    (obj/notify! obj :expandable-closed)
    ))

(defn toggle [this]
  (if (open? this)
    (close this)
    (open this)))

(defn close-others [this]
  (doseq [child (obj/get-children-of-type (obj/get-parent this) this)]
    (when (and (not= child this) (open? child))
      (close child))
   ))

(defn node [oref]
  (let [dom-fn (obj/get-type-attr oref :expandable-dom)
        state-attr (obj/get-type-attr oref :expandable-attr)]
    (when-not state-attr
      (throw (str "object:" (obj/-type oref) " did not define :expandable-attr")))
    (when-not dom-fn
      (throw (str "object:" (obj/-type oref) " did not define :expandable-dom cant use expandable/node without")))

    (obj/bind oref state-attr
              (fn [v]
                (if v
                  (obj/make-dom oref :expandable-dom :expandable-events)
                  "" ;; empty string produces an invisible Text Node on the dom which serves as a nice placeholder :P
                  )))))


