(ns shadow.debug
  (:require [shadow.object :as so]
            [shadow.ui.expandable :as expandable]
            [shadow.ui.popup :as popup]
            [shadow.keyboard :as kb]
            [shadow.dom :as dom]
            [shadow.inspect :as inspect]
            ))

(def debug-bar (atom nil))

;; public actions
(defn list-objects [this]
  (popup/open this ::debug-popup {:items (vals @so/instances)}))

;; obj defs
(so/define ::item
  :defaults {:show-details false}

  :dom (fn [this]
         [:li [:div.title (str (:item this))]
          (expandable/node this)
          ])

  :on [:expandable-opening expandable/close-others]

  :expandable-attr :show-details

  :expandable-dom (fn [this]
                    [:div.item-details
                     [:h3 (get-in this [:item :shadow.object/object-name])]
                     (inspect/inspect @(:item this))
                     ])

  :expandable-events []

  :dom/events [[:click ".title"] expandable/toggle])


(so/define ::debug-popup
  :dom (fn [this]
         [:div#debug-items {:style "z-index: 65000; position: fixed; top: 0px; left: 0px; right: 0px; bottom: 0px;"}
          [:a.close {:href "#"} "close"]
          [:h2 "Object Snapshot [Items #" (so/bind-simple this :items count) "]"]
          [:p
           [:a.refresh {:href "#"} "refresh"]]
          (so/bind-children [:ul.items]
                             this :items
                             ::item :item)])

  :dom/events [[:click "a.close"] popup/close
               [:click "a.refresh"] #(so/update! % assoc :items (vals @so/instances))]
  )

(so/define ::debug-bar
  :defaults {:object-count 0}

  :dom (fn [this]
         [:div#debug-bar
          [:div.object-count "Object Count: #" (so/bind-simple this :object-count) " "]])

  :keyboard ["shift-escape" #(list-objects %)]

  :dom/events [:click list-objects])

(defn ^:export activate! []
  (let [bar (so/create ::debug-bar {})]
    (reset! debug-bar bar)

    (add-watch so/instances 'obj-count-watch (fn [_ _ _ new]
                                                (so/update! bar assoc :object-count (count new))
                                                ))

    (kb/push-focus bar)

    (dom/append bar)
    ))
