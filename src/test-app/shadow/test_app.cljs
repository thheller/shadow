(ns shadow.test-app
  (:require-macros [cljs.core.async.macros :refer (go)])
  (:require [cljs.core.async :as async]
            [shadow.components :as sc :refer ($ <$ defc)]
            [shadow.util :as util :refer (log with-timing)]
            [shadow.api :as api :refer (ns-ready)]
            [shadow.html :as html]
            [shadow.dom :as dom]
            ))

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
    (get-in @root path))
  IWatchable
  (-add-watch [this key callback]
    (-add-watch root key callback))
  (-remove-watch [this key]
    (-remove-watch root key)))

(defc object-display
  :triggers [:object]

  :dom (fn [{:keys [object] :as this} _]
         ($ html/div
            ($ html/h2 "OBJECT!")
            ($ div-title (<$ object object-title))
            (<$ [object :i] str))))

(defc toolbar
  :dom (fn [this children]
         ($ html/div "toolbar: " children)))

(defc yo
  :triggers [test-data]
  
  :init (fn [this])

  :dom/init (fn [this el])
  
  :dom (fn [this _]
         ($ html/div
            ($ html/h1
               "hello"
               (<$ [test-data [:level :i] str])
               (<$ [test-data :name]))

            ($ html/div
               "yo"

               (<$ [test-data [:level :i]] str)
               
               (<$ [test-data :object]
                   {:key :id
                    :dom (fn [object]
                           (when object
                             ($ (object-display {:object (Cursor. test-data [:object])}
                                                (fn [el scope]
                                                  (go (let [ret-val (<! el)]
                                                        (log "object display died" ret-val))))

                                                (fn [el scope]
                                                  (sc/update! this assoc-in [:refs :display] el))))
                             ))})
               
               ($ (toolbar
                   #_ (sc/tap [:channels :select] chan or (fn [toolbar msg mult ch]))
                   #_ (sc/pipe [:channels :select] chan or (fn [toolbar msg ch]))
                   )

                  ($ (html/button 
                      (sc/on :click #(swap! test-data update-in [:level :i] inc)))
                     "inc level")

                  ($ (html/button
                      (sc/on :click #(swap! test-data assoc :object {:id 1
                                                                     :name "obj1"
                                                                     :i 0})))
                     "swap obj 1") 
                  
                  

                  ($ (html/button
                      (sc/on :click #(swap! test-data dissoc :object)))
                     "remove obj") 

                  ($ (html/button
                      (sc/on :click #(swap! test-data assoc :object {:id 2
                                                                     :name "obj2"
                                                                     :i 0})))
                     "swap obj 2")

                  ($ (html/button
                      (sc/on :click (fn [e el]
                                      (let [ref (get-in this [:refs :display])]
                                        (log "inc i" ref)
                                        (when ref
                                          (swap! test-data update-in [:object :i] inc))))))
                     "inc i")) 

 
               ))))

(defn ^:export start [ref]
  (log "START")
  
  (let [root (sc/construct yo)]
    (reset! root-c root)
    (dom/insert-before ref root)))

(defn ^:export stop []
  (log "STOP")

  (when-let [r @root-c]
    (sc/destroy! r)
    (reset! root-c nil)))

(ns-ready)