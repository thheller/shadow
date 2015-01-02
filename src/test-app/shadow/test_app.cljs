(ns shadow.test-app
  (:require-macros [shadow.macros :refer (log ns-ready with-timing)])
  (:require [shadow.components :as c :refer ($ <$ defc) :refer-macros (defc)]
            [shadow.html :as html]
            [shadow.dom :as dom]))

(def test-data (atom {:name "Thomas"
                      :level {:i 0}}))

(def root-c (atom nil))

(def my-h1 (html/h1 {:class "special"}))

(defc yo
  :on [:init (fn [this]
               (log "called init of yo" this))]

  :dom (fn [this _]
         ($ html/div
            ($ html/h1 "hello" (<$ [test-data [:level :i] str]) (<$ [test-data :name]))
            ($ html/p
               "yo"
               (<$ [test-data [:level :i]] str)
               
               ($ (html/button {:class "hello-world"}
                   (c/on :click #(swap! test-data update-in [:level :i] inc)))
                  "click me!")
               ))))

(str "hello" 1 "world" :yo nil)


(def render-queued (atom false))

(defn frame-fn []
  (let [start (.getTime (js/Date.))]
    (reset! render-queued false)
    (c/process-frame! c/root-scope)
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
  
  (let [root (c/construct yo)]
    (reset! root-c root)
    (dom/insert-before ref root)))

(defn ^:export stop []
  (log "STOP")
  
  (stop-frames! :my-app test-data)
  (when-let [r @root-c]
    (c/destroy! r)
    (reset! root-c nil)))

(ns-ready)
