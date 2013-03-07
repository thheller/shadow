(ns shadow.ui.expandable
  (:require [shadow.object :as so]
            [shadow.dom :as dom]))


(so/define-event :expandable-opening "duh?" [])
(so/define-event :expandable-opened "duh?" [])
(so/define-event :expandable-closing "duh?" [])
(so/define-event :expandable-closed "duh?" [])

(comment
  (so/define-property :expandable-attr "which obj attribute should be used to toggle the expandable (should be a boolen) example :expandable-attr [:showing]")
  (so/define-property :expandable-dom "function which creates the expandable dom")
  (so/define-property :expandable-events "dom events which will be attached to the dom created by :expandable-dom"))

(defn- get-state-attr [obj]
  (so/get-type-attr obj :expandable-attr))

(defn open? [obj]
  (let [state-attr (get-state-attr obj)]
    (get obj state-attr)))

(defn open [obj]
  (when-not (open? obj)
    (so/notify! obj :expandable-opening)
    (so/update! obj assoc (get-state-attr obj) true)
    (so/notify! obj :expandable-opened)))

(defn close [obj]
  (when (open? obj)
    (so/notify! obj :expandable-closing)
    (so/update! obj assoc (get-state-attr obj) false)
    (so/notify! obj :expandable-closed)
    ))

(defn toggle [this]
  (if (open? this)
    (close this)
    (open this)))

(defn close-others [this]
  (doseq [child (so/get-children-of-type (so/get-parent this) this)]
    (when (and (not= child this) (open? child))
      (close child))
   ))

(defn node [oref]
  (let [dom-fn (so/get-type-attr oref :expandable-dom)
        state-attr (so/get-type-attr oref :expandable-attr)]
    (when-not state-attr
      (throw (str "object:" (so/-type oref) " did not define :expandable-attr")))
    (when-not dom-fn
      (throw (str "object:" (so/-type oref) " did not define :expandable-dom cant use expandable/node without")))

    ;; FIXME: this does not cleanup properly
    ;; should use bind with a custom object
    (so/bind-simple oref state-attr
              (fn [v]
                (if v
                  (so/make-dom oref :expandable-dom :expandable-events)
                  "" ;; empty string produces an invisible Text Node on the dom which serves as a nice placeholder :P
                  )))))


