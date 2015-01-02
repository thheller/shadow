(ns shadow.components
  {:load-macros true}
  (:require-macros [shadow.components :as sc])
  (:require [clojure.string :as str]
            [goog.string :as gstr]
            [shadow.object :as so]
            [shadow.util :as util :refer (log)]
            [shadow.dom :as dom]))

(def next-id
  (let [id-seq (atom 0)]
    (fn []
      (swap! id-seq inc))))

(defprotocol IElementFactory
  (-create-element [this scope children]))

(defprotocol IDynamicElement
  (-insert-element! [this scope el]))

(defprotocol IElementList
  (-get-elements [this]))

(defprotocol IConstruct
  (-construct [this scope]))

(defprotocol IScoped
  (-get-scope [this]))

(defprotocol IDestructable
  (destroy! [this]))

(defprotocol IUpdate
  (-update! [this update-fn]))

(defprotocol IFramed
  (process-frame! [this]))

(defprotocol IPull
  (-pull! [this] "basically IDeref"))

(defn update!
  ([target target-fn]
     (-update! target target-fn))
  ([target target-fn a1]
     (-update! target #(target-fn % a1)))
  ([target target-fn a1 a2]
     (-update! target #(target-fn % a1 a2)))
  ([target target-fn a1 a2 a3]
     (-update! target #(target-fn % a1 a2 a3)))
  ([target target-fn a1 a2 a3 a4]
     (-update! target #(target-fn % a1 a2 a3 a4)))
  ([target target-fn a1 a2 a3 a4 a5]
     (-update! target #(target-fn % a1 a2 a3 a4 a5)))
  ([target target-fn a1 a2 a3 a4 a5 a6]
     (-update! target #(target-fn % a1 a2 a3 a4 a5 a6)))
  ([target target-fn a1 a2 a3 a4 a5 a6 & more]
     (-update! target #(apply target-fn % a1 a2 a3 a4 a5 a6 more))))

(defn expand-children! [parent scope children]
  (loop [x children]
    (when (seq x)
      (let [[it & more] x]
        (cond
         ;; didn't want recursive cause we might nest deeply
         ;; which would make the stack bigger than required
         (satisfies? IElementList it)
         (recur (concat (-get-elements it) more))

         (satisfies? IDynamicElement it)
         (do (-insert-element! it scope parent)
             (recur more))
         
         :else
         (let [el (-construct it scope)]
           (dom/append parent el)
           (recur more)
           ))))))



(def childless
  #{"IMG"
    "INPUT"})

(defn dom-element [tag scope attrs init-fns children]
  (when (and (contains? childless tag)
             (seq children))
    (throw (ex-info (str tag " cannot have children, yet we got some") {:tag tag
                                                                        :attrs attrs
                                                                        :children children})))
  
  (let [el (js/document.createElement tag)]
    (dom/set-attrs el attrs)

    (doseq [init-fn init-fns]
      (init-fn el scope nil))

    (expand-children! el scope children)
    el
    ))

(deftype ElementFactory [el-ctor el-attr el-init]
  IFn
  (-invoke [_ attr]
    (ElementFactory. el-ctor (merge el-attr attr) el-init))
  (-invoke [_ attr i1]
    (ElementFactory. el-ctor (merge el-attr attr) (conj el-init i1)))
  (-invoke [_ attr i1 i2]
    (ElementFactory. el-ctor (merge el-attr attr) (conj el-init i1 i2)))
  (-invoke [_ attr i1 i2 i3]
    (ElementFactory. el-ctor (merge el-attr attr) (conj el-init i1 i2 i3)))
  (-invoke [_ attr i1 i2 i3 i4]
    (ElementFactory. el-ctor (merge el-attr attr) (conj el-init i1 i2 i3 i4)))
  (-invoke [_ attr i1 i2 i3 i4 i5]
    (ElementFactory. el-ctor (merge el-attr attr) (conj el-init i1 i2 i3 i4 i5)))
  (-invoke [_ attr i1 i2 i3 i4 i5 i6]
    (ElementFactory. el-ctor (merge el-attr attr) (conj el-init i1 i2 i3 i4 i5 i6)))
  (-invoke [_ attr i1 i2 i3 i4 i5 i6 i7]
    (ElementFactory. el-ctor (merge el-attr attr) (conj el-init i1 i2 i3 i4 i5 i6 i7)))
  (-invoke [_ attr i1 i2 i3 i4 i5 i6 i7 i8]
    (ElementFactory. el-ctor (merge el-attr attr) (conj el-init i1 i2 i3 i4 i5 i6 i7 i8)))
  ;; FIXME: add more -invoke ...
  
  IDeref
  (-deref [_] el-ctor)

  IConstruct
  (-construct [^not-native this scope]
    (-create-element this scope #js []))

  IElementFactory
  (-create-element [_ scope children]
    (el-ctor scope el-attr el-init children)))

(defn- process-all! [items]
  (reduce (fn [_ item] (process-frame! item)) nil items))

(defn filter-by-id [current filter-id]
  (->> current
       (remove #(= filter-id (.-id %)))
       (into [])))

(deftype Scope [id ^:mutable alive? actions children]

  IDestructable
  (destroy! [this]
    (set! alive? false)
    (doseq [[_ child] @children]
      (destroy! child)
      ))

  IFramed
  (process-frame! [this]
    (process-all! @actions)
    (process-all! @children))
  
  Object
  (addAction [this action]
    (swap! actions conj action))
  (removeAction [this action]
    (swap! actions filter-by-id (.-id action)))
  (addChild [this child]
    (swap! children conj child))
  (removeChild [this child]
    (swap! children filter-by-id (.-id child))))

(deftype ScopeAction [id ^:mutable alive? scope val observable callback]
  IFramed
  (process-frame! [this]
    ;; an action or scope might be destroyed mid-frame, we don't need to render it then
    (when (and (.-alive? scope) alive?)
      (let [old-val @val
            new-val (-pull! observable)]
        (when (and (not (identical? old-val new-val))
                   (not= old-val new-val))
          (.do! this old-val new-val)
          ))))
  
  IDestructable
  (destroy! [this]
    (set! alive? false)
    (.removeAction scope this))
  
  Object
  (execute! [this]
    (.do! this @val (-pull! observable)))

  (do! [this old-val new-val]
    (reset! val new-val)
    (callback this old-val new-val)))

(defn scope-action [scope observable callback]
  (let [action (ScopeAction. (next-id) true scope (atom nil) observable callback)]
    (.addAction scope action)
    action))

(deftype ScopedElement [scope el]
  IScoped
  (-get-scope [_]
    scope)

  IDestructable
  (destroy! [_]
    (dom/remove el)
    (destroy! scope))

  dom/IElement
  (-to-dom [_] el))

(def next-scope-id
  (let [id (atom 0)]
    (fn []
      (str "$$scope" (swap! id inc)))))

(def next-component-id
  (let [id (atom 0)]
    (fn [prefix]
      (str "$$" prefix "$" (swap! id inc)))))

(defn new-scope
  [parent]
  (let [scope (Scope. (next-scope-id)
                      true
                      (atom [])
                      (atom []))]
    (when parent
      (.addChild parent scope))
    scope
    ))

(defn $bind
  "whenever the observable value changes, do (callback el new-value)
   where el is the current dom element"
  [observable outer-callback]
  (fn [el scope owner]
    (let [callback (fn [action old-val new-val]
                     (outer-callback el new-val))]

      (.execute! (scope-action scope observable callback))
      )))

(defn $bind-attr [attr observable callback]
  ($bind observable (fn [el new-value] (dom/set-attr el attr (callback new-value)))))

(defn on [event callback]
  (fn [el scope owner]
    (dom/on el event callback)))

(deftype DynamicElement [observable ctor]
  IDynamicElement
  (-insert-element! [_ scope outer-el]
    (let [current-el (atom nil)

          callback (fn [action old-val new-val]
                     (let [old-el @current-el]
                       (when (and old-el (satisfies? IDestructable old-el))
                         (destroy! old-el))
                     
                       (let [new-def (ctor new-val)
                             ;; if no node needs to be constructed at this point
                             ;; just use empty text node as a placeholder
                             new-el (-construct (if (nil? new-def) "" new-def) scope)]

                         (reset! current-el new-el)

                         (if old-el
                           (dom/replace-node old-el new-el)
                           (dom/append outer-el new-el))
                         )))]
      
      (.execute! (scope-action scope observable callback))
      )))

(defn <$
  "<$ read as \"at this position in the tree\""
  ([observable]
     (DynamicElement. observable identity))
  ([observable ctor]
     (DynamicElement. observable ctor)))

(defonce root-scope (new-scope nil))

(defn construct [el]
  (-construct el root-scope))

(deftype CaseEl [observable branches]
  IDynamicElement
  (-insert-element! [_ scope outer-el]
    (let [current (atom nil)
          
          callback (fn [action old-val new-val]
                     (let [old @current]
                       (when old
                         (destroy! old))
                     
                       (let [new-branch (branches new-val)
                             new-el (-construct (let [new-def (new-branch)]
                                                  (if (nil? new-def) "" new-def))
                                                scope)]

                         (reset! current new-el)
                       
                         (if old
                           (dom/replace-node old new-el)
                           (dom/append outer-el new-el))
                         )))]
      
      (.execute! (scope-action scope observable callback))
      )))

(defn case-el* [observable branches]
  (CaseEl. observable branches))

(extend-protocol IElementList
  cljs.core/IndexedSeq
  (-get-elements [this] this)
  cljs.core/LazySeq
  (-get-elements [this] this))

(extend-protocol IPull
  cljs.core/PersistentVector
  (-pull! [[source key]]
    (if (sequential? key)
      (get-in @source key)
      (get @source key))))

(extend-protocol IConstruct
  #_ cljs.core/PersistentVector
  #_ (-construct [el scope]
       (let [[factory & children] el]
         (when-not (satisfies? IElementFactory factory)
           (throw (ex-info (str "invalid vector in tree, must start with factory, got " (type factory)) {:el el})))
         (-create-element factory scope children)))

  string
  (-construct [el scope]
    (js/document.createTextNode el))
  
  number
  (-construct [el scope]
    (js/document.createTextNode (str el))))

;; BACKWARDS COMPATIBILTY HACK, SHOULD NOT BE USED!
(extend-protocol IElementFactory
  cljs.core/Keyword
  (-create-element [kw scope children]
    (let [[tag id class] (dom/parse-tag kw)]
      (dom-element tag
                   scope
                   (-> {}
                       (cond->
                        id (assoc :id id)
                        class (assoc :class class)))
                   []
                   children))))

(extend-protocol IDynamicElement
  cljs.core/PersistentArrayMap
  (-insert-element! [this scope el]
    (dom/set-attrs el this))
  cljs.core/PersistentHashMap
  (-insert-element! [this scope el]
    (dom/set-attrs el this)))

(deftype ComponentInstance [id type spec scope state ^:mutable dom]
  IScoped
  (-get-scope [_]
    scope)

  IDestructable
  (destroy! [_]
    (when dom
      (dom/remove dom))
    (destroy! scope))
  
  IDeref
  (-deref [_]
    @state)
  
  IWatchable
  (-add-watch [_ key callback]
    (-add-watch state key callback))
  (-remove-watch [_ key]
    (-remove-watch state key))
  
  IUpdate
  (-update! [_ update-fn]
    (swap! state update-fn))
  
  ILookup
  (-lookup [_ key]
    (get @state key))
  (-lookup [_ key default]
    (get @state key default))

  dom/IElement
  (-to-dom [_] dom))

(defn fire-event! [obj ev & args]
  ;; (log "fire-event!" obj ev args)
  )

(defn mutual-destruction! [scope cmp]
  ;; (log "mutual destruct" scope cmp)
  )

(defn create-instance
  [spec parent-scope attr init-fns children]
  (let [scope (new-scope parent-scope)
        id (next-component-id (:name spec))
        cmp (ComponentInstance. id (:name spec) spec scope (atom attr) nil)
        
        _ (do (mutual-destruction! scope cmp) 
              (fire-event! cmp :init))

        dom-fn (:dom spec)
        tree (dom-fn cmp children)
        ;; _ (log (:name spec) tree)
        dom (-construct tree scope)]

    (dom/set-data dom :cid id)
    (set! (.-dom cmp) dom)
    (aset dom "$$owner" cmp)

    (doseq [init-fn init-fns]
      (init-fn dom scope cmp))
    
    (fire-event! cmp :dom/init)

    cmp))

(defn optimize-spec [spec]
  spec)


(deftype NodeBuilder [el-factory children]
  IConstruct
  (-construct [_ scope]
    (-create-element el-factory scope children)
    ))

(defn $
  ([el]
     (NodeBuilder. el #js []))
  ([el c1]
     (NodeBuilder. el #js [c1]))
  ([el c1 c2]
     (NodeBuilder. el #js [c1 c2]))
  ([el c1 c2 c3]
     (NodeBuilder. el #js [c1 c2 c3]))
  ([el c1 c2 c3 c4]
     (NodeBuilder. el #js [c1 c2 c3 c4]))
  ([el c1 c2 c3 c4 c5]
     (NodeBuilder. el #js [c1 c2 c3 c4 c5]))
  ([el c1 c2 c3 c4 c5 c6]
     (NodeBuilder. el #js [c1 c2 c3 c4 c5 c6])))
