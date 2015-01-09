(ns shadow.components
  (:require-macros [shadow.components :as m]
                   [cljs.core.async.macros :refer (go alt!)])
  (:require [cljs.core.async :as async]
            [cljs.core.async.impl.protocols :as async-protocols]
            [goog.dom.animationFrame.polyfill :as raf-polyfill]

            [clojure.string :as str]
            [goog.string :as gstr]
            
            [shadow.util :as util :refer (log doarray)]
            [shadow.dom :as dom]))

(raf-polyfill/install)

(def next-id
  (let [id-obj #js {:id 0}]
    (fn []
      (set! (.-id id-obj) (inc (.-id id-obj)))
      )))

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

(defprotocol IDestruct
  (destroy! [this]))

(defprotocol IUpdate
  (-update! [this update-fn]))

(defprotocol IFramed
  (process-frame! [this]))

(defprotocol ISlice
  (-slice [this path]))

(defprotocol IRefCounted
  (-ref-inc! [this])
  (-ref-dec! [this]))

(declare queue-frame! update!)

(deftype Cursor [id parent path]
  IDeref
  (-deref [_]
    (get-in @parent path))
  
  IRefCounted
  (-ref-inc! [this]
    (-ref-inc! parent))

  (-ref-dec! [this]
    (-ref-dec! parent))
  
  IUpdate
  (-update! [this update-fn]
    (update! parent update-in path update-fn))

  IEquiv
  (-equiv [this other]
    (and (instance? Cursor other)
         (= id (.-id other))))

  ISlice
  (-slice [this new-path]
    (let [id (next-id)]
      (Cursor. id this new-path)))

  IHash
  (-hash [this]
    (js/goog.getUid this))
  
  IPrintWithWriter
  (-pr-writer [_ w opts]
    (-write w "#<Cursor ")
    (-pr-writer path w opts)
    (-write w " ")
    (-pr-writer parent w opts)
    (-write w ">"))
  
  Object
  (toString [this]
    (str "#<Cursor " path " " parent ">")))

(deftype RootCursor [id root ^:mutable ref-count]
  IDeref
  (-deref [_] @root)

  ISlice
  (-slice [this new-path]
    (let [id (next-id)]
      (Cursor. id this new-path)))
  
  IUpdate
  (-update! [this update-fn]
    (swap! root update-fn))
  
  IRefCounted
  (-ref-inc! [this]
    (when (zero? ref-count)
      ;; if something references us and our value changes we need to queue a frame
      (add-watch root id (fn [_ _ _ _] (queue-frame!))))
    (set! ref-count (inc ref-count)))
  (-ref-dec! [this]
    (set! ref-count (dec ref-count))
    (when (zero? ref-count)
      (remove-watch root id)))

  IPrintWithWriter
  (-pr-writer [_ w opts]
    (-write w "#<RootCursor :ref-count ")
    (-write w (str ref-count))
    (-write w ">")))

(defn slice [src path]
  (when-not (satisfies? ISlice src)
    (throw (ex-info "cannot slice into src" {:src src :path path})))
  (if (sequential? path)
    (-slice ^not-native src path)
    (-slice ^not-native src [path])))

(defn root-cursor [atom]
  (RootCursor. (next-id) atom 0))

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
         (nil? it)
         (recur more)
         ;; didn't want recursive cause we might nest deeply
         ;; which would make the stack bigger than required
         (satisfies? IElementList it)
         (recur (concat (-get-elements it) more))

         (satisfies? IDynamicElement it)
         (do (-insert-element! it scope parent)
             (recur more))
         
         (array? it)
         (recur (concat it more))

         :else
         (let [el (-construct it scope)]
           (dom/append parent el)
           (recur more)
           ))))))


;; FIXME: do this correctly or not at all.
#_ (def childless #{"IMG" "INPUT"})

(defn dom-element [tag scope attrs init-fns children]
  #_ (when (and (contains? childless tag)
                (seq children))
       (throw (ex-info (str tag " cannot have children, yet we got some") {:tag tag
                                                                           :attrs attrs
                                                                           :children children})))
  
  (let [el (js/document.createElement tag)]
    (dom/set-attrs el attrs)

    (doseq [init-fn init-fns]
      (init-fn el scope))

    (expand-children! el scope children)
    el
    ))

(deftype ElementFactory [el-ctor el-attr el-init]
  IFn
  (-invoke [this]
    this)
  (-invoke [_ attr]
    (if (map? attr)
      (ElementFactory. el-ctor (merge el-attr attr) el-init)
      (ElementFactory. el-ctor el-attr (conj el-init attr))))
  (-invoke [_ attr i1]
    (if (map? attr)
      (ElementFactory. el-ctor (merge el-attr attr) (conj el-init i1))
      (ElementFactory. el-ctor el-attr (conj el-init attr i1))))
  (-invoke [_ attr i1 i2]
    (if (map? attr)
      (ElementFactory. el-ctor (merge el-attr attr) (conj el-init i1 i2))
      (ElementFactory. el-ctor el-attr (conj el-init attr i1 i2))))
  (-invoke [_ attr i1 i2 i3]
    (if (map? attr)
      (ElementFactory. el-ctor (merge el-attr attr) (conj el-init i1 i2 i3))
      (ElementFactory. el-ctor el-attr (conj el-init attr i1 i2 i3))))
  (-invoke [_ attr i1 i2 i3 i4]
    (if (map? attr)
      (ElementFactory. el-ctor (merge el-attr attr) (conj el-init i1 i2 i3 i4))
      (ElementFactory. el-ctor el-attr (conj el-init attr i1 i2 i3 i4))))
  (-invoke [_ attr i1 i2 i3 i4 i5]
    (if (map? attr)
      (ElementFactory. el-ctor (merge el-attr attr) (conj el-init i1 i2 i3 i4 i5))
      (ElementFactory. el-ctor el-attr (conj el-init attr i1 i2 i3 i4 i5))))
  (-invoke [_ attr i1 i2 i3 i4 i5 i6]
    (if (map? attr)
      (ElementFactory. el-ctor (merge el-attr attr) (conj el-init i1 i2 i3 i4 i5 i6))
      (ElementFactory. el-ctor el-attr (conj el-init attr i1 i2 i3 i4 i5 i6))))
  (-invoke [_ attr i1 i2 i3 i4 i5 i6 i7]
    (if (map? attr)
      (ElementFactory. el-ctor (merge el-attr attr) (conj el-init i1 i2 i3 i4 i5 i6 i7))
      (ElementFactory. el-ctor el-attr (conj el-init attr i1 i2 i3 i4 i5 i6 i7))))
  (-invoke [_ attr i1 i2 i3 i4 i5 i6 i7 i8]
    (if (map? attr)
      (ElementFactory. el-ctor (merge el-attr attr) (conj el-init i1 i2 i3 i4 i5 i6 i7 i8))
      (ElementFactory. el-ctor el-attr (conj el-init attr i1 i2 i3 i4 i5 i6 i7 i8))))
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
  (doseq [item items]
    (process-frame! item)))

(defn filter-by-id [current filter-id]
  (->> current
       (reduce (fn [next item]
                 (if (= filter-id (.-id item))
                   next
                   (conj! next item)))
               (transient []))
       (persistent!)))


;; naming things is hard, Scope and ScopeAction suck!
(deftype Scope [id name parent ^:mutable alive? ^:mutable owner actions children]
  IDestruct
  (destroy! [this]
    (when alive?
      (set! alive? false)
      (when owner
        (destroy! owner))
      (doseq [child @children]
        (destroy! child))
      (doseq [action @actions]
        (destroy! action))
      (when parent
        (.removeChild parent this))
      ))

  IFramed
  (process-frame! [this]
    (process-all! @actions)
    (process-all! @children))
  
  Object
  (addAction [this action]
    (when-not alive?
      (throw (ex-info "can't add action to dead scope" {:scope this :action action})))
    (swap! actions conj action))
  (removeAction [this action]
    (when alive?
      (swap! actions filter-by-id (.-id action))))
  (addChild [this child]
    (when-not alive?
      (throw (ex-info "can't add children to dead scope" {:scope this :child child}))) 
    (swap! children conj child))
  (removeChild [this child]
    (when alive?
      (swap! children filter-by-id (.-id child)))))

(deftype ScopeAction [id ^:mutable alive? scope val cursor callback]
  IFramed
  (process-frame! [this]
    ;; an action or scope might be destroyed mid-frame, we don't need to render it then
    (when (and alive? (.-alive? scope))
      (let [old-val @val
            new-val @cursor]
        (when (not= old-val new-val)
          (.do! this old-val new-val)))))
  
  IDestruct
  (destroy! [this]
    (when alive?
      (set! alive? false)
      (-ref-dec! cursor)
      (.removeAction scope this)))
  
  Object
  (execute! [this]
    (.do! this @val (-deref cursor)))

  (do! [this old-val new-val]
    (reset! val new-val)
    (callback this old-val new-val)))

(def next-scope-id
  (let [id (atom 0)]
    (fn []
      (str "$$scope" (swap! id inc)))))

(defn new-scope
  [name parent]
  (let [id (next-scope-id)
        scope (Scope. id name parent true nil (atom []) (atom []))]
    (when parent
      (.addChild parent scope))
    scope
    ))

;; RENDER
;; FIXME: processing does not always need to start at root scope
;; the watch should instead add its scope to the list of scopes needing an update
;; on render we should then sort those and only render the ones that require an update
;; sorting parents before children is important
(defonce root-scope (new-scope "ROOT" nil))

(def render-queued (atom false))

(defn frame-fn []
  (let [start (.getTime (js/Date.))]
    (reset! render-queued false)
    (process-frame! root-scope)
    (let [frame-time (- (.getTime (js/Date.)) start)]
      (when (> frame-time 16)
        (log "LONG FRAME TIME!" frame-time))
      )))

(defn queue-frame! []
  (when-not @render-queued
    (reset! render-queued true)
    (js/window.requestAnimationFrame frame-fn)))

;; /RENDER

(defn scope-action [scope cursor callback]
  (when (nil? cursor)
    (throw (ex-info "cannot construct action without cursor" {})))
  (let [id (next-id)
        action (ScopeAction. id true scope (atom nil) cursor callback)]
    (.addAction scope action)
    (-ref-inc! cursor)
    action))


(defn bind
  "whenever the cursor value changes, do (callback el new-value)
   where el is the current dom element"
  [cursor outer-callback]
  (fn [el scope owner]
    (let [callback (fn [action old-val new-val]
                     (outer-callback el new-val))]

      (.execute! (scope-action scope cursor callback))
      )))

(defn bind-attr [attr cursor callback]
  (bind cursor (fn [el new-value] (dom/set-attr el attr (callback new-value)))))

(defn on [event callback]
  (fn [el scope owner]
    (dom/on el event (fn [e ev-el]
                       (callback e el scope owner)))))

(deftype DynamicElement [cursor opts]
  IDynamicElement
  (-insert-element! [_ outer-scope outer-el]
    (let [current (atom nil)
          
          ;; introduce middlemen scope so we keep our position in the tree
          ;; otherwise swapping a child takes us from to the end
          inner-scope (new-scope "<$" outer-scope)
          
          {:keys [key insert replace update remove dom]} opts

          callback (fn [action old-val new-val]
                     (let [old-el @current]

                       ;; FIXME: add special case where (key new-val) returns nil but new-val is not nil
                       ;; update is not correct since we might not have an element yet
                       (if (and old-el key (= (key old-val) (key new-val)))
                         ;; if the key stays the same don't fully swap the object
                         (update outer-el old-el old-val new-val)
                         
                         (let [new-def (dom cursor)
                               ;; if no node needs to be constructed at this point
                               ;; just use empty text node as a placeholder
                               new-el (-construct (if (nil? new-def) "" new-def) inner-scope)]

                           (reset! current new-el)

                           (cond
                            (nil? old-el)
                            (insert outer-el new-el new-val)
                            (nil? new-def)
                            (remove outer-el old-el old-val new-el)
                            :else
                            (replace outer-el old-el old-val new-el new-val))))))]
      
      (.execute! (scope-action inner-scope cursor callback))
      )))

(def <$-default-opts
  {:insert (fn [outer-el new-el new-val]
             (dom/append outer-el new-el))
   :replace (fn [outer-el old-el old-val new-el new-val]
              (dom/replace-node old-el new-el)
              (when (satisfies? IDestruct old-el)
                (destroy! old-el)))
   
   :update (fn [outer-el el old-val new-val]) ;; NO-OP

   :remove (fn [outer-el old-el old-val placeholder]
             (dom/replace-node old-el placeholder) 
             (when (satisfies? IDestruct old-el)
               (destroy! old-el)))
   :dom (fn [cursor] @cursor)})

(defn <$
  "<$ read as \"at this position in the tree\""
  ([cursor]
     (when (or (nil? cursor)
               (not (satisfies? IDeref cursor))
               (not (satisfies? IRefCounted cursor)))
       (throw (ex-info "cannot construct <$ without cursor" {:cursor cursor})))
     (DynamicElement. cursor <$-default-opts))

  ([cursor opts]
     (when (or (nil? cursor)
               (not (satisfies? IDeref cursor))
               (not (satisfies? IRefCounted cursor)))
       (throw (ex-info "cannot construct <$ without cursor" {:cursor cursor})))
     (let [opts (cond
                 (map? opts)
                 (merge <$-default-opts opts)
                 (fn? opts)
                 (assoc <$-default-opts :dom opts)
                 :else
                 (throw (ex-info "invalid argument to dynamic element" {:opts opts})))]

       (DynamicElement. cursor opts))))

(deftype KeyedCollection [outer-cursor opts]
  IDynamicElement
  (-insert-element! [_ outer-scope outer-el]
    (let [{:keys [key dom]} opts
          inner-scope (new-scope "<$*" outer-scope)
            
          outer-dom (dom/dom-node outer-el)

          key->idx (volatile! {})
          idx->key (volatile! [])
          idx->item (volatile! [])
            
          marker (js/document.createTextNode "")
          _ (.appendChild outer-dom marker)
            
          make-el (fn keyed-collection-make-el [item item-cursor]
                    (let [item-def (dom item-cursor)]
                      (-construct (if (nil? item-def) "" item-def) inner-scope)
                      ))

          callback (fn keyed-collection-frame [action old-val new-val]
                     (when-not (vector? new-val)
                       (throw (ex-info "not a vector" {:new-val new-val})))

                     (if (nil? old-val)
                       ;; first run only?
                       (doseq [[idx item] (map-indexed vector new-val)]
                         (let [item-cursor (slice outer-cursor [idx])
                               item-key (key item)
                               item-el (make-el item item-cursor)]
                           (vswap! key->idx assoc item-key idx)
                           (vswap! idx->key assoc idx item-key)
                           (vswap! idx->item assoc idx item-cursor)
                           (dom/append outer-el item-el)
                           ))

                       (let [child-nodes (.-childNodes outer-dom)
                             offset (inc (dom/index-of marker))
                             new-c (count new-val)
                             prev-c (count old-val)
                             diff (- new-c prev-c)]
                           
                         (when (neg? diff)
                           (let [new-keys (into #{} (map key new-val))
                                 destroy-these (array)]
                             (doseq [[key idx] @key->idx]
                               (when-not (contains? new-keys key)
                                 (let [prev-el (aget child-nodes (+ offset idx))]
                                   (vswap! key->idx dissoc key)
                                   (vswap! idx->item util/remove-from-vector idx)
                                   (vswap! idx->key util/remove-from-vector idx)
                                   ;; must delay removal since we access child-nodes which is mutable
                                   ;; alternative would be to put child-nodes into a vector first
                                   (.push destroy-these prev-el)
                                   )))

                             (doarray [it destroy-these]
                                      (if-let [owner (aget it "$$owner")]
                                        (destroy! owner)
                                        (dom/remove it)))
                               

                             ;; FIXME: protocolize this somehow
                             ;; changing the path of the cursor cause the may have been moved
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
                               (let [item-cursor (slice outer-cursor [idx])
                                     item-el (make-el item item-cursor)]
                                   
                                 (vswap! key->idx dissoc prev-key)
                                 (vswap! idx->key assoc idx item-key)
                                 (vswap! idx->item assoc idx item-cursor)
                                 (vswap! key->idx assoc item-key idx)
                                 (dom/replace-node current-el item-el)
                                 (when-let [owner (aget current-el "$$owner")]
                                   (destroy! owner))))))

                         (when (pos? diff)
                           (loop [idx prev-c
                                  new-items (subvec new-val prev-c)
                                  last-el (aget child-nodes (+ offset idx -1))]
                             (when (seq new-items)
                               (let [item (first new-items)
                                     item-key (key item)
                                     item-cursor (slice outer-cursor [idx])
                                     item-el (make-el item item-cursor)]
                                 (vswap! idx->item assoc idx item-cursor)
                                 (vswap! idx->key assoc idx item-key)
                                 (vswap! key->idx assoc item-key idx)
                                 (dom/insert-after last-el item-el)
                                 (recur (inc idx) (rest new-items) item-el)
                                 )))))))]
        
      (.execute! (scope-action inner-scope outer-cursor callback)))
    ))

(defn <$*
  "takes a cursor to a vector and create/destroys dom nodes accoring to content
   will call :dom for each item in the vector"
  [outer-cursor opts]
  (KeyedCollection. outer-cursor opts))

(defn construct
  ([el]
     (-construct el root-scope))
  ([scope el]
     (let [scope (cond
                  (instance? Scope scope)
                  scope
                  (satisfies? IScoped scope)
                  (-get-scope scope)
                  :else
                  (throw (ex-info "invalid scope" {:scope scope :el el})))]
       (-construct el scope))))

(deftype CaseEl [cursor branches]
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
      
      (.execute! (scope-action scope cursor callback))
      )))

(defn case-el* [cursor branches]
  (CaseEl. cursor branches))

(extend-protocol IElementList
  cljs.core/IndexedSeq
  (-get-elements [this] this)
  cljs.core/LazySeq
  (-get-elements [this] this))

(extend-protocol IConstruct
  cljs.core/PersistentVector
  (-construct [el scope]
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

(deftype Instance
    [id
     type
     spec
     scope
     chan
     ^:mutable ret-val
     state ;; local state
     ^:mutable dom
     ^:mutable alive?
     ^:mutable ref-count]
  IScoped
  (-get-scope [_]
    scope)

  IDestruct
  (destroy! [this]
    (when alive?
      (set! alive? false)

      (set! (.-owner scope) nil)
      (destroy! scope)
      
      (when dom
        (aset (dom/dom-node dom) "$$owner" nil)
        (dom/remove dom))
      
      ;; FIXME: should we remove dom first?
      (when ret-val
        (async/put! chan ret-val))
      (async/close! chan)
      ))
  
  ISlice
  (-slice [this path]
    (Cursor. (next-id) this path))
  
  IDeref
  (-deref [_]
    @state)
  
  IRefCounted
  (-ref-inc! [this]
    (when (zero? ref-count)
      (add-watch state id (fn [_ _ _ _] (queue-frame!))))
    (set! ref-count (inc ref-count)))
  (-ref-dec! [this]
    (set! ref-count (dec ref-count))
    (when (zero? ref-count)
      (remove-watch state id)))

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
  (-to-dom [_] dom)

  async-protocols/ReadPort
  (take! [_ fn1-handler]
    (async-protocols/take! chan fn1-handler)))

(defn call [instance handle & args]
  (let [spec (.-spec instance)
        handle-fn (get spec handle)]
    (when handle-fn
      (apply handle-fn instance args))))

(defn call! [instance handle & args]
  (let [spec (.-spec instance)
        handle-fn (get spec handle)]
    (when-not handle-fn
      (throw (ex-info "component does not provide call handle" {:handle handle
                                                                :instance instance
                                                                :args args})))
    (apply handle-fn instance args)))

(defn set-ref [target path]
  (fn [el scope]
    (let [current (get-in @target path)]
      (update! target assoc-in path el)
      (call target :ref-changed path current el)
      )))

(def next-component-id
  (let [id (atom 0)]
    (fn [prefix]
      (str "$$" prefix "$" (swap! id inc)))))

(defn create-instance
  [spec parent-scope attr init-fns children]
  (let [scope (new-scope (:name spec) parent-scope)
        id (next-component-id (:name spec))
        
        ;; FIXME: change to promise-chan when we get them!
        ;; there may be many other instances waiting for our death
        ;; all should get the return val, not just one lucky one
        chan (async/chan 1)
        
        cmp (Instance. id (:name spec) spec scope chan nil (atom attr) nil true 0)]
    
    ;; FIXME: if any of this fails we leak an Instance
    ;;        which is semi constructed but may already have watches & stuff
    ;;        wrapping in try/catch prevents JIT from optimizing?

    (set! (.-owner scope) cmp)
    (call cmp :init)

    (let [dom (or (let [dom-fn (:dom spec)]
                    (when dom-fn
                      (-construct (dom-fn cmp children) scope)))
                  (let [dom-fn (:dom attr)]
                    (when (and dom-fn (fn? dom-fn))
                      (-construct (dom-fn cmp children) scope)))
                  (let [dom (:dom attr)]
                    (cond
                     (and dom (satisfies? dom/IElement dom))
                     dom

                     (and dom (satisfies? IConstruct dom))
                     (do (when (seq children)
                           (throw (ex-info "given :dom arg ignores children" {:dom dom :children children})))
                         (-construct dom scope))
                     :else
                     nil)))]
      
      (when-not dom
        (throw (ex-info "component did not construct a dom node" {:spec spec :attr attr})))

      (dom/set-data dom :cid id)
      (set! (.-dom cmp) dom)
      (aset dom "$$owner" cmp)

      (doseq [init-fn init-fns]
        (init-fn cmp scope))
      
      (call cmp :dom/init (dom/dom-node dom))

      cmp)))

(defn return-and-destroy! [cmp ret-val]
  (when-not (instance? Instance cmp)
    (throw (ex-info "only component instances know how to return values" {:cmp cmp})))
  (set! (.-ret-val cmp) ret-val)
  (destroy! cmp))


(deftype NodeBuilder [el-factory children]
  IConstruct
  (-construct [_ scope]
    (-create-element el-factory scope children)
    ))

(defn check-el [el]
  (when-not (satisfies? IElementFactory el)
    (throw (ex-info "invalid element, must start with element factory" {:el el}))))

(defn $
  ([el]
     (check-el el)
     (NodeBuilder. el #js []))
  ([el c1]
     (check-el el)
     (NodeBuilder. el #js [c1]))
  ([el c1 c2]
     (check-el el)
     (NodeBuilder. el #js [c1 c2]))
  ([el c1 c2 c3]
     (check-el el)
     (NodeBuilder. el #js [c1 c2 c3]))
  ([el c1 c2 c3 c4]
     (check-el el)
     (NodeBuilder. el #js [c1 c2 c3 c4]))
  ([el c1 c2 c3 c4 c5]
     (check-el el)
     (NodeBuilder. el #js [c1 c2 c3 c4 c5]))
  ([el c1 c2 c3 c4 c5 c6]
     (check-el el)
     (NodeBuilder. el #js [c1 c2 c3 c4 c5 c6]))
  ([el c1 c2 c3 c4 c5 c6 c7]
     (check-el el)
     (NodeBuilder. el #js [c1 c2 c3 c4 c5 c6 c7]))
  ([el c1 c2 c3 c4 c5 c6 c7 c8]
     (check-el el)
     (NodeBuilder. el #js [c1 c2 c3 c4 c5 c6 c7 c8]))
  ([el c1 c2 c3 c4 c5 c6 c7 c8 c9]
     (check-el el)
     (NodeBuilder. el #js [c1 c2 c3 c4 c5 c6 c7 c8 c9]))
  ([el c1 c2 c3 c4 c5 c6 c7 c8 c9 c10]
     (check-el el)
     (NodeBuilder. el #js [c1 c2 c3 c4 c5 c6 c7 c8 c9 c10]))
  ([el c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11]
     (check-el el)
     (NodeBuilder. el #js [c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11]))
  ([el c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12]
     (check-el el)
     (NodeBuilder. el #js [c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12]))
  )
