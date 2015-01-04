(ns shadow.test-app
  (:require-macros [cljs.core.async.macros :refer (go alt!)])
  (:require [cljs.core.async :as async]
            [shadow.components :as sc :refer ($ <$ defc)]
            [shadow.util :as util :refer (log with-timing)]
            [shadow.api :as api :refer (ns-ready)]
            [shadow.html :as html]
            [shadow.dom :as dom]
            [shadow.animate :as anim]
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

(defc toast
  :dom/init (fn [this dom]
              (dom/on dom :click #(sc/destroy! this)))

  :dom (fn [this children]
         ($ (html/div {:class "my-toast"}) children)))

;; FIXME: currently toasts can overlap each other, should instead queue after each other
(defn display-toast
  [parent
   {:keys [timeout]
    :or {timeout 3000}
    :as attr}
   & body]
  (let [toast (sc/construct parent ($ (toast attr) body))
        slide-in (anim/setup 80 {toast (anim/combine
                                        (anim/transition :transform "translateY(100%)" "translateY(0)" "ease-in-out")
                                        (anim/fade-in "ease-in-out"))})]

    (anim/init! slide-in)
    (dom/append toast) 
    (anim/continue! slide-in)

    (let [timeout (async/timeout timeout)]
      (go (alt!
            toast
            ([_] :toast-died)
            ;; if toast is still alive, we remove it
            timeout
            ([_] (sc/destroy! toast))
            )))))

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
                      (sc/on :click (fn [e el]
                                      (display-toast this {} "hello world"))))
                     "toast")
                  

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
    (dom/insert-before ref root)
    
    (display-toast root {} ($ html/h2 "Welcome!"))))

(defn ^:export stop []
  (log "STOP")

  (when-let [r @root-c]
    (sc/destroy! r)
    (reset! root-c nil)))

(ns-ready)
