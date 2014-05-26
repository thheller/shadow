(ns shadow.ui.popup
  (:require-macros [shadow.macros :refer (log)])
  (:require [shadow.object :as so]
            [shadow.keyboard :as kb]
            [shadow.dom :as dom]
            [shadow.ui :as ui]))

(so/define-event :popup-closing "" [])
(so/define-event :popup-closed "" [])
(so/define-event :popup-open "" [])

(def body (.. js/document -body))

(def active-popups (atom 0))

(defn close [this]
  (let [parent (so/get-parent this)]
    (so/notify! parent :popup-closing)
    (so/destroy! this)
    (so/notify! parent :popup-closed)))

(so/define ::popup-backdrop
  :on [:init (fn [this]
               (dom/add-class body "modal-visible"))
       
       :dom/init (fn [this]
                   (dom/set-style this {:z-index (* 10000 (::level this))}))

       :destroy (fn [this]
                  (dom/remove-class body "modal-visible"))]

  :dom (fn [this] [:div#backdrop])
  :dom/events [:click #(close (:parent %))]

  :keyboard ["escape" #(close (:parent %))])

(defn do-center-on-screen [popup]
  (let [{vw :w vh :h} (dom/get-viewport-size)
        {sw :w sh :h} (dom/get-size popup)
        mx (int (/ (- vw sw) 2))
        my (int (/ (- vh sh) 2))]
    (dom/set-style popup {:top (str my "px")
                          :left (str mx "px")})))

;; behavior yes/no?
;;
;; :on [:popup-open popup/center-on-screen]
;; vs
;; :behaviors [popup/center-on-screen]
(def ^{:doc "behavior"} center-on-screen
  [:popup-open do-center-on-screen])

(defn create [parent popup-type obj]
  (when-not (map? obj)
    (throw (ex-info "popup/create requires map as third arg" {:obj obj})))
  (so/create popup-type (merge obj {:parent parent})))

(defn show [popup]
  (let [;; make backdrop a child of the popup, so it gets destroyed when the popup is destroyed, neat eh?
        backdrop (so/create ::popup-backdrop {:parent popup
                                              ::level (::level popup)})]

    (kb/push-focus backdrop)
    (kb/push-focus popup)

    (dom/append backdrop)
    (dom/append popup)

    (dom/set-style popup {:opacity 0})
    (dom/set-style backdrop {:opacity 0})

    (so/notify-tree! popup :dom/entered)

    (ui/with-timeout 1
      (fn []
        (dom/add-class backdrop "popup-ease-in")
        (dom/add-class popup "popup-ease-in")
        (dom/set-style backdrop {:opacity 0.6})
        (dom/set-style popup {:opacity 1})))

    (so/notify! popup :popup-open)
    popup
    ))

(defn open [parent popup-type args]
  (swap! active-popups inc)
  (let [lvl @active-popups
        popup (create parent popup-type (assoc args
                                          ::level lvl))]
    (so/add-reaction! popup :destroy (fn []
                                       (swap! active-popups dec)))
    
    (dom/set-style popup {:z-index (+ 1 (* 10000 lvl))}) 
    
    (show popup)))

