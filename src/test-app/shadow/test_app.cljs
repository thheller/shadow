(ns shadow.test-app
  (:require [shadow.components :as sc :refer ($ <$ defc)]
            [shadow.util :as util :refer (log with-timing)]
            [shadow.api :as api :refer (ns-ready)]
            [shadow.html :as html]
            [shadow.dom :as dom]))

(def test-data (atom {:name "Thomas"
                      :level {:i 0}}))

(def root-c (atom nil))

(def my-h1
  (html/h1 {:class "special"}))

(def div-title
  (html/div {:class "title"}))

(defn object-title
  [{:keys [id name] :as object}]
  (str "object #" id " - " name))

(deftype Cursor [root path]
  sc/IPull
  (-pull! [_]
    (get-in @root path))
  IDeref
  (-deref [_]
    (get-in @root path)))

(defc object-display
  :dom (fn [{:keys [object] :as this} _]
         ($ html/div
            ($ html/h2 "OBJECT!")
            ($ div-title (<$ object object-title))
            (<$ [object :i] str))))

(defc yo
  :on [:init (fn [this]
               (log "called init of yo" this))]

  :dom (fn [this _]
         ($ html/div
            ($ html/h1 "hello" (<$ [test-data [:level :i] str]) (<$ [test-data :name]))
            ($ html/p
               "yo"
               (<$ [test-data [:level :i]] str)
               
               (<$ [test-data :object]
                   {:key :id
                    :dom (fn [object observable]
                           (when object
                             ($ (object-display {:object (Cursor. test-data [:object])}))
                             ))})
               
               ($ (html/button 
                   {}
                   (sc/on :click #(swap! test-data update-in [:level :i] inc)))
                  "inc level")

               ($ (html/button
                   {}
                   (sc/on :click #(swap! test-data assoc :object {:id 1
                                                                  :name "obj1"
                                                                  :i 0})))
                  "swap obj 1") 

               ($ (html/button
                   {}
                   (sc/on :click #(swap! test-data dissoc :object)))
                  "remove obj") 

               ($ (html/button
                   {}
                   (sc/on :click #(swap! test-data assoc :object {:id 2
                                                                  :name "obj2"
                                                                  :i 0})))
                  "swap obj 2")

               ($ (html/button
                   {}
                   (sc/on :click #(swap! test-data update-in [:object :i] inc)))
                  "inc i") 

 
               ))))

(def render-queued (atom false))

(defn frame-fn []
  (let [start (.getTime (js/Date.))]
    (reset! render-queued false)
    (sc/process-frame! sc/root-scope)
    (let [frame-time (- (.getTime (js/Date.)) start)]
      (when (> frame-time 16)
        (log "LONG FRAME TIME!" frame-time))
      )))

(defn start-frames! [key root]
  (add-watch root key (fn [_ _ _ _]
                        (when-not @render-queued
                          (reset! render-queued true)
                          (js/window.requestAnimationFrame frame-fn)))))

(defn stop-frames! [key root]
  (remove-watch root key))

(defn ^:export start [ref]
  (log "START")
  (start-frames! :my-app test-data)
  
  (let [root (sc/construct yo)]
    (reset! root-c root)
    (dom/insert-before ref root))

  )



(defn ^:export stop []
  (log "STOP")
  
  (stop-frames! :my-app test-data)
  (when-let [r @root-c]
    (sc/destroy! r)
    (reset! root-c nil)))

(ns-ready)
