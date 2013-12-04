(ns shadow.route
  (:import goog.History)
  (:require [clojure.data :as data]
            [goog.events :as gev]
            [shadow.dom :as dom]
            [shadow.object :as so]
            [clojure.string :as str]
            ))


;; FIXME: rewrite this, its clearly broken in some places

(def history (History.))
(def current-state (atom nil))

(def current-path (atom (-> js/document
                            (.-location)
                            (.-pathname))))

;; why would u ever want an empty token, "/" is way saner
(defn fix-token [token]
  (if (= token "") "/" token))


(so/define-event :route/activate "sent when routing is complete" [])

(so/define-event :route/begin "sent before routing is started"
  [[:path "the path we will route to"]])

(so/define-event :route/done "sent when routing is done"
  [[:state "the new top dog"]])

(so/define-event :route/pop "sent when a route is left, followed immediantly by destroy!" [])

(so/define-event :route/push "sent to the parent before the new child is made master"
  [[:new-child "the new child"]])

(so/define-event :route/navigate "direct routing"
  [[:route "new route value"]])

(defn pop-current []
  (let [current @current-state
        parent (so/get-parent current)]
    (so/notify! current :route/pop)
    (so/destroy! current)
    (reset! current-state parent)))

(defn route-match? [parts input-tokens]
  (when (<= (count parts) (count input-tokens))
    (loop [parts parts
           tokens input-tokens
           route-depth 0
           route-args {}]
      (if (empty? parts)
        [(assoc route-args
           ::route-depth route-depth
           ::route-tokens (vec (take route-depth input-tokens)))
         tokens]
        (let [pf (first parts)
              tf (first tokens)]
          (cond
           (keyword? pf)
           (recur (rest parts) (rest tokens) (inc route-depth) (assoc route-args pf tf))
           (= pf tf)
           (recur (rest parts) (rest tokens) (inc route-depth) route-args)
           :else
           nil)
          )))))

(defn enter-route [route-type route-args]
  (let [current @current-state
        child (so/create route-type (assoc route-args :parent current))
        parent-dom (so/get-dom current)
        child-container (dom/query-one ".route-children" parent-dom)]
    (so/debug "enter-route" route-type route-args)
    (when-not child-container
      (throw (str "route " (pr-str current) " does not have a .route-children in its dom, please add")))

    (so/dom-enter child-container child)
    (so/notify! current-state :route/push child)
    (reset! current-state child)))

(defn push-routes [tokens]
  (when (seq tokens)
    (let [current @current-state
          child-routes (so/get-type-attr current :routes)]
      (loop [routes (partition 2 child-routes)]
        (if (empty? routes)
          (throw (str "failed to route" (pr-str tokens)))

          (let [[route-parts route-type] (first routes)
                route-parts (if (vector? route-parts) route-parts [route-parts])]
            (if-let [[route-args remaining] (route-match? route-parts tokens)]
              (do
                (enter-route route-type route-args)
                (when (seq remaining)
                  (push-routes remaining)))
              (recur (rest routes))
              )))))))



(defn tokenize [path]
  (let [path (if (= "/" (first path))
               (.substring path 1)
               path)]
    (.split path "/")
    ))

(defn get-current-path []
  (loop [current @current-state
         result []]
    (if-let [tokens (::route-tokens current)]
      (recur (:parent current) (conj result tokens))
      (str "/" (str/join "/" (flatten (reverse result))))
      )))

(defn starts-with [a b]
  (= 0 (.indexOf a b)))

(defn reroute [path]
  (so/notify-up! @current-state :route/begin path)
  
  (loop [current-path (get-current-path)]
    (so/log "reroute" {:path path
                       :current current-path})
    (if (= path current-path)
     true
     (if (starts-with path current-path)
       (let [new-tokens (tokenize (.substring path (count current-path)))]
         (push-routes new-tokens)
         (.setToken history path))

       ;; else need to drop down
       (do
         (pop-current)
         (recur (get-current-path))))))

  (so/notify-up! @current-state :route/done @current-state)
  (so/notify! @current-state :route/activate)
  )

;; called from the app itself, should maybe do some extra checks?
(defn navigate! [path]
  (reroute path))

(defn push-state!
  "set a new url token without doing any routing"
  [path]
  (.setToken history path))

(defn replace-state!
  "replace current url in history"
  [path]
  (.replaceToken history path))

(defn get-state []
  (.getToken history))

(defn intercept-clicks-on-a [e]
  (let [target (.-target e)]
    (when (= "A" (.-nodeName target))
      (dom/ev-stop e)
      (reroute (.substring (.-hash target) 1))
      )))

(defn init [root-state]
  ;; (.setUseFragment history true) ;; Html5History only
  (.setEnabled history true)

  (reset! current-state root-state)

  ;; (dom/on (dom/dom-node root-state) :click intercept-clicks-on-a)

  (gev/listen history "navigate"
              (fn [e]
                (when (.-isNavigation e)
                  (reroute (fix-token (.-token e)))
                  )))

  (let [token (fix-token (.getToken history))]
    (reroute token)))


(defn register [handler]
  (.setEnabled history true)

  (let [key (gev/listen history "navigate"
                        (fn [e]
                          (when (.-isNavigation e)
                            (let [new-token (.-token e)]
                              (so/notify! handler :route/navigate new-token)))))]

    (so/add-reaction! handler :destroy
                      (fn [this]
                        (gev/unlistenByKey key)
                        ))
    
    (let [token (.getToken history)]
      (so/notify! handler :route/navigate token))

    key))
