(ns shadow.test-app
  (:require-macros [cljs.core.async.macros :refer (go alt!)])
  (:require [cljs.core.async :as async]
            [shadow.components :as sc :refer ($ <$ defc)]
            [shadow.util :as util :refer (log with-timing)]
            [shadow.api :as api :refer (ns-ready)]
            [shadow.html :as html]
            [shadow.dom :as dom]
            [shadow.animate :as anim]
            [shadow.material.toast :as toast]
            [shadow.material.ripple :as ripple]
            [shadow.dev-tools :as dt]
            ))

(defn object-title
  [{:keys [id name] :as object}]
  (str "object #" id " - " name))

(def div-title
  (html/div {:class "title"}))

(def div-form-group
  (html/div {:class "form-group"}))

(def btn-default
  (html/button
   {:type "button" :class "btn btn-default"}))

(defc object-display
  :dom (fn [{:keys [object-c] :as this} _]
         ($ html/div
            ($ html/h2 "OBJECT!")
            ($ div-title (<$ object-c object-title))
            (<$ (sc/cursor object-c [:i]))
            )))

(defn inc-clicks [data e el]
  (sc/update! data update :clicks inc))

(defn clicks-text [clicks]
  (cond
   (zero? clicks)
   "No clicks? :("

   (= clicks 1)
   "Only once?"

   (< clicks 25)
   (str clicks " Clicks!")
   
   :else
   (str clicks " Clicks! Sir Click-A-Lot!")
   ))

(defn dom-index-of [el]
  (loop [el (dom/dom-node el)
         i 0]
    (let [ps (.-previousSibling el)]
      (if (nil? ps)
        i
        (recur ps (inc i))
        ))))

(defn <$* [cursor opts]
  (reify
    sc/IDynamicElement
    (-insert-element! [_ outer-scope outer-el]
      (let [{:keys [key dom]} opts
            inner-scope (sc/new-scope "<$*" outer-scope)
            
            outer-dom (dom/dom-node outer-el)

            key->idx (volatile! {})
            idx->key (volatile! [])
            idx->item (volatile! [])
            
            marker (js/document.createTextNode "")
            _ (.appendChild outer-dom marker)
            
            make-el (fn [item item-cursor]
                      (let [item-def (dom item item-cursor)]
                        (sc/-construct (if (nil? item-def) "" item-def) inner-scope)
                        ))

            callback (fn [action old-val new-val]
                       (when-not (vector? new-val)
                         (throw (ex-info "not a vector" {:new-val new-val})))

                       (if (nil? old-val)
                         ;; first run only?
                         (doseq [[idx item] (map-indexed vector new-val)]
                           (let [item-cursor (sc/cursor cursor [idx])
                                 item-key (key item)
                                 item-el (make-el item item-cursor)]
                             (vswap! key->idx assoc item-key idx)
                             (vswap! idx->key assoc idx item-key)
                             (vswap! idx->item assoc idx item-cursor)
                             (dom/append outer-el item-el)
                             ))

                         (let [child-nodes (.-childNodes (dom/dom-node outer-el))
                               offset (inc (dom-index-of marker))
                               new-c (count new-val)
                               prev-c (count old-val)
                               diff (- new-c prev-c)]
                           
                           (when (neg? diff)
                             (let [new-keys (into #{} (map key new-val))]
                               (doseq [[key idx] @key->idx]
                                 (when-not (contains? new-keys key)
                                   (let [prev-el (aget outer-dom "childNodes" (+ offset idx))]
                                     (vswap! key->idx dissoc key)
                                     (vswap! idx->item util/remove-from-vector idx)
                                     (vswap! idx->key util/remove-from-vector idx)
                                     (if-let [owner (aget prev-el "$$owner")]
                                       (sc/destroy! owner)
                                       (dom/remove prev-el)))))
                               
                               (doseq [[idx child-cursor] (map-indexed vector @idx->item)]
                                 (set! (.-path child-cursor) [idx])
                                 )))

                           ;; FIXME: very naive implementation
                           ;; could move nodes arround if their position changed
                           ;; now just destroys/creates nodes if keys don't match
                           ;; works well enough for now
                           (dotimes [idx (min prev-c new-c)]
                             (let [current-el (aget child-nodes (+ offset idx))
                                   item (nth new-val idx)
                                   item-key (key item)
                                   prev-key (get @idx->key idx)]
                               (if (= item-key prev-key)
                                 ;; item-key didn't change, but index may have changed
                                 (do (when (not= idx (get @key->idx item-key))
                                       (vswap! key->idx assoc item-key idx))
                                     nil)
                                 ;; item-key changed, swap in new item, destroy old
                                 (let [item-cursor (sc/cursor cursor [idx])
                                       item-el (make-el item item-cursor)]
                                   
                                   (vswap! key->idx dissoc prev-key)
                                   (vswap! idx->key assoc idx item-key)
                                   (vswap! idx->item assoc idx item-cursor)
                                   (vswap! key->idx assoc item-key idx)
                                   (dom/replace-node current-el item-el)
                                   (when-let [owner (aget current-el "$$owner")]
                                     (sc/destroy! owner))))))

                           (when (pos? diff)
                             (loop [idx prev-c
                                    new-items (subvec new-val prev-c)
                                    last-el (aget child-nodes prev-c)]
                               (when (seq new-items)
                                 (let [item (first new-items)
                                       item-key (key item)
                                       item-cursor (sc/cursor cursor [idx])
                                       item-el (make-el item item-cursor)]
                                   (vswap! idx->item assoc idx item-cursor)
                                   (vswap! idx->key assoc idx item-key)
                                   (vswap! key->idx assoc item-key idx)
                                   (dom/insert-after last-el item-el)
                                   (recur (inc idx) (rest new-items) item-el)
                                   )))))))]
        
        (.execute! (sc/scope-action inner-scope cursor callback)))
      )))

(defc coll-item-view
  :dom (fn [{:keys [item] :as this}]
         ($ (html/li
             (sc/on :click #(sc/update! item update-in [:x] inc)))
            (<$ (sc/cursor item :name))
            " x: "
            (<$ (sc/cursor item :x))
            )))

(defc test-component
  :init (fn [this])

  :dom/init (fn [this el])
  
  :dom (fn [{:keys [data] :as this} _]
         (let [object-c (sc/cursor data :object)]
           ($ html/div
              ($ html/h1
                 "Hello "
                 (<$ (sc/cursor data [:name])
                     (fn [value cursor]
                       (if (seq value)
                         value
                         "Stranger")))
                 "!")
              
              ($ (html/button
                  (sc/on :click #(let [now (.getTime (js/Date.))]
                                   (sc/update! data update-in [:coll] conj {:id now
                                                                            :name (str "item" now)}))))
                 "add item")

              ($ (html/button
                  (sc/on :click #(let [now (.getTime (js/Date.))]
                                   (sc/update! data update-in [:coll] (fn [coll]
                                                                        (assoc coll
                                                                          (int (rand (count coll)))
                                                                          {:id now
                                                                           :name (str "item" now)}))))))
                 "replace random item")

              ($ (html/button
                  (sc/on :click #(let [now (.getTime (js/Date.))]
                                   (sc/update! data update-in [:coll] (fn [coll]
                                                                        (let [idx (int (rand (count coll)))]
                                                                          (util/remove-from-vector coll idx))
                                                                        )))))
                 "remove random item")

              ($ (html/button
                  (sc/on :click #(let [now (.getTime (js/Date.))]
                                   (sc/update! data update-in [:coll] (fn [coll] (into [] (rest coll)))))))
                 "remove first item")

              ($ (html/button
                  (sc/on :click #(let [now (.getTime (js/Date.))]
                                   (sc/update! data update-in [:coll] (fn [coll] (into [] (butlast coll)))))))
                 "remove last item")

              ($ html/ul
                 (<$* (sc/cursor data :coll)
                      {:key :id
                       :dom (fn [data cursor]
                              (coll-item-view {:item cursor}))}))
              

              ($ (html/form
                  (sc/on :submit (fn [e el]
                                   (dom/ev-stop e)
                                   (log "cancelled submit" e el))))

                 ($ div-form-group
                    ($ html/label "What is your name?")

                    (html/input
                     {:class "form-control" :placeholder "..." :autofocus true}
                     (sc/on :keyup (fn [e el]
                                     (sc/update! data assoc :name (dom/get-value el))))))
                 ($ div-form-group
                    ($ (btn-default
                        (ripple/for-element this)
                        (sc/on :click (partial inc-clicks data)))
                       "Click me, I do Stuff!"))

                 ($ div-form-group
                    (<$ (sc/cursor data [:clicks]) clicks-text)))
              

              ($ html/div

                 ($ (btn-default
                     (sc/on :click (fn [e el]
                                     (toast/display this {} "Hello World!"))))
                    "toast") 

                 ($ (btn-default
                     (sc/on :click #(sc/update! data assoc :object {:id 1
                                                                    :name "obj1"
                                                                    :i 0})))
                    "swap obj 1") 
                 
                 
                 ($ (btn-default
                     (sc/on :click #(sc/update! data assoc :object {:id 2
                                                                    :name "obj2"
                                                                    :i 0})))
                    "swap obj 2")

                 ($ (btn-default
                     (sc/on :click (fn [e el]
                                     (let [ref (get-in this [:refs :display])]
                                       (when ref
                                         (sc/update! data update-in [:object :i] inc))))))
                    "inc i")

                 ($ (btn-default
                     (sc/on :click #(sc/update! data dissoc :object)))
                    "remove obj")

                 ($ (btn-default
                     (sc/on :click dt/scope-snapshot))
                    "scope snapshot"))
              
              (<$ object-c
                  {:key :id
                   :dom (fn [data cursor]
                          (when data
                            ($ (object-display {:object-c cursor}
                                               (sc/set-ref this [:refs :display])
                                               (fn [el scope]
                                                 (go (<! el)
                                                     (log "object display died")))
                                               ))
                            ))})

              
              ))))

(def test-data (atom {:name ""
                      :clicks 0
                      :coll (into [] (for [i (range 50)]
                                       {:id i
                                        :name (str "item" i)
                                        :x 0}))}))

(def root-c (atom nil))

(defn ^:export start [ref]
  (log "START")

  (let [root (sc/construct (test-component {:data (sc/root-cursor test-data)}))]
    (reset! root-c root)
    (dom/insert-before ref root)

    ;; (toast/display root {} ($ html/h2 "Welcome!"))
    ))

(defn ^:export stop []
  (log "STOP")

  (when-let [r @root-c]
    (sc/destroy! r)
    (reset! root-c nil)))

(ns-ready)
