(ns shadow.debug
  (:require [shadow.object :as obj]
            [shadow.ui.expandable :as expandable]
            [shadow.ui.popup :as popup]
            [shadow.keyboard :as kb]
            [shadow.dom :as dom]
            [shadow.inspect :as inspect]
            ))

(def debug-bar (atom nil))

;; public actions
(defn list-objects [this]
  (popup/open this ::debug-popup {:items (vals @obj/instances)}))

;; obj defs
(obj/define ::item
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

  :dom-events [[:click ".title"] expandable/toggle])


(obj/define ::debug-popup
  :dom (fn [this]
         [:div#debug-items.popup.fullscreen
          [:a.close {:href "#"} "close"]
          [:h2 "Object Snapshot [Items #" (obj/bind this :items count) "]"]
          [:p
           [:a.refresh {:href "#"} "refresh"]]
          (obj/bind-children [:ul.items]
                             this :items
                             ::item :item)])

  :dom-events [[:click "a.close"] popup/close
               [:click "a.refresh"] #(obj/update! % assoc :items (vals @obj/instances))]
  )

(obj/define ::debug-bar
  :defaults {:object-count 0}

  :dom (fn [this]
         [:div#debug-bar
          [:span.object-count "Object Count: #" (obj/bind this :object-count) " "]
          [:b "DEBUG "]
          [:button#debug-list.btn {:type "button"} "Show Object Snapshot"]])

  :keyboard ["shift-escape" #(list-objects %)]

  :dom-events [[:click "#debug-list"] list-objects])

(defn ^:export activate! []
  (let [bar (obj/create ::debug-bar {})]
    (reset! debug-bar bar)

    (add-watch obj/instances 'obj-count-watch (fn [_ _ _ new]
                                                (obj/update! bar assoc :object-count (count new))
                                                ))

    (kb/push-focus bar)

    (dom/append bar)
    ))
