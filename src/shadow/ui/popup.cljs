(ns shadow.ui.popup
  (:require [shadow.object :as obj]
            [shadow.keyboard :as kb]
            [shadow.dom :as dom]))

(obj/define-event :popup-closing "" [])
(obj/define-event :popup-closed "" [])
(obj/define-event :popup-open "" [])

(defn close [this]
  (let [parent (obj/get-parent this)]
    (obj/notify! parent :popup-closing)
    (obj/destroy! this)
    (obj/notify! parent :popup-closed)))

(obj/define ::popup-backdrop
  :on []
  :dom (fn [this] [:div#backdrop])
  :dom-events [:click #(close (:parent %))]

  :keyboard ["escape" #(close (:parent %))])

(defn create [parent popup-type obj]
  (when-not (map? obj)
    (throw (ex-info "popup/create requires map as third arg" {:obj obj})))
  (obj/create popup-type (merge obj {:parent parent})))

(defn show [popup]
  (let [;; make backdrop a child of the popup, so it gets destroyed when the popup is destroyed, neat eh?
        backdrop (obj/create ::popup-backdrop {:parent popup})]

    (kb/push-focus backdrop)
    (kb/push-focus popup)

    (dom/append backdrop)
    (dom/append popup)
    (obj/notify! popup :popup-open)
    popup
    ))

(defn open [parent popup-type obj]
  (show (create parent popup-type obj)))

