(ns shadow.components
  (:refer-clojure :exclude (meta map))
  (:require-macros [shadow.macros :as sm :refer (log)]
                   [shadow.components :as sc])
  (:require [clojure.string :as str]
            [goog.string :as gstr]
            [shadow.object :as so]
            [shadow.dom :as dom]))

(defprotocol IElementFactory
  (-create-element [this children]))

(defprotocol IDynamicElement
  (-insert-element! [this el]))

(defprotocol IElementList
  (-get-elements [this]))

(defprotocol IConstructElement
  (construct [el]))

(defprotocol IScoped
  (-get-scope [this]))

(defprotocol IDestructable
  (destroy! [this]))

(defprotocol IObservable
  (-pull [this] "return the current state")
  (-subscribe! [this sub-key callback])
  (-unsubscribe! [this sub-key]))

(defn apply-attributes! [el attrs]
  (let [el (dom/dom-node el)]
    ;; basically clone of goog.dom.setProperties, but with keywords
    (doseq [[key value] attrs]
      (case key
        :id (set! (.-id el) (str value))
        :class (set! (.-className el) (str value))
        :for (set! (.-htmlFor el) value)
        ;; see goog.dom.DIRECT_ATTRIBUTE_MAP_
        :cellpadding (.setAttribute el "cellPadding" value)
        :cellspacing (.setAttribute el "cellSpacing" value)
        :colspan (.setAttribute el "colSpan" value)
        :frameborder (.setAttribute el "frameBorder" value)
        :height (.setAttribute el "height" value)
        :maxlength (.setAttribute el "maxLength" value)
        :role (.setAttribute el "role" value)
        :rowspan (.setAttribute el "rowSpan" value)
        :type (.setAttribute el "type" value)
        :usemap (.setAttribute el "useMap" value)
        :valign (.setAttribute el "vAlign" value)
        :width (.setAttribute el "width" value)
        ;; FIXME: support :style maps
        (let [ks (name key)]
          (if (or (gstr/startsWith ks "data-")
                  (gstr/startsWith ks "aria-"))
            (.setAttribute el ks value)
            (aset el ks value)))))))

(defn expand-children! [parent children]
  (loop [x children]
    (when (seq x)
      (let [[it & more] x]
        (cond
         ;; didn't want recursive cause we might nest deeply
         ;; which would make the stack bigger than required
         (satisfies? IElementList it)
         (recur (concat (-get-elements it) more))

         (satisfies? IDynamicElement it)
         (do (-insert-element! it parent)
             (recur more))
         
         :else
         (let [el (construct it)]
           (dom/append parent el)
           (recur more)
           ))))))

(def childless
  #{"IMG"
    "INPUT"})

(defn dom-element [tag attrs init-fns children]
  (when (and (contains? childless tag)
             (seq children))
    (throw (ex-info (str tag " cannot have children, yet we got some") {:tag tag
                                                                        :attrs attrs
                                                                        :children children})))

  (let [el (js/document.createElement tag)]
    (apply-attributes! el attrs)
    (doseq [init-fn init-fns]
      (init-fn el nil))
    (expand-children! el children)
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

  IElementFactory
  (-create-element [_ children]
    (el-ctor el-attr el-init children)))

(deftype Scope [id parent ^:mutable subs ^:mutable children]
  Object
  (addSubscription [this scope-key observable]
    (set! subs (assoc subs scope-key observable)))
  (removeSubscription [this scope-key]
    (set! subs (dissoc subs scope-key)))
  (addChild [this child]
    (set! children (assoc children (.-id child) child)))
  (removeChild [this child]
    (set! children (dissoc children (.-id child))))

  IScoped
  (-get-scope [this] this)
  
  IDestructable
  (destroy! [this]
    (log "destroying scope" id)
    (when parent
      (.removeChild parent this))
    (doseq [[_ child] children]
      (destroy! child))
    (set! children {})
    (doseq [[key obs] subs]
      (-unsubscribe! obs key))
    (set! subs {})
    ))

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

(def ^:dynamic *scope* nil)

(defn new-scope
  ([] (new-scope *scope*))
  ([parent]
     (let [scope (Scope. (next-scope-id) parent {} {})]
       (when parent
         (.addChild parent scope))
       scope
       )))

(deftype ScopeKey [scope key]
  IEquiv
  (^boolean -equiv [_ other]
    (and (instance? ScopeKey other)
         (-equiv key (.-key other))))

  IHash
  (-hash [_]
    (-hash key)))

(def gen-scope-key
  (let [id-seq (atom 0)]
    (fn []
      (when (nil? *scope*)
        (throw (ex-info "cannot create key without scope" {})))

      (ScopeKey. *scope* (str "$$scope-key" (swap! id-seq inc))))))

(defn subscribe! [observable key callback]
  (let [scope (.-scope key)]
    (when (nil? scope)
      (throw (ex-info "cannot subscribe! without a scope" {})))
    
    (-subscribe! observable key callback)
    (.addSubscription scope key observable)
    ))

(defn unsubscribe! [observable key]
  (let [scope (.-scope key)]
    (when-not scope
      (throw (ex-info "key without a scope trying to unsub" {})))

    (.removeSubscription scope key)
    (-unsubscribe! observable key)))

(defn $bind
  "whenever the observable value changes, do (callback el new-value)
   where el is the current dom element"
  [observable callback]
  (fn [el owner]
    (let [sub-key (gen-scope-key)
          sub-fn (fn sub-fn [new]
                   (callback el new))]

      (subscribe! observable sub-key sub-fn)
      (sub-fn (-pull observable))
      nil)))

(defn $bind-attr [attr observable callback]
  ($bind observable (fn [el new-value] (dom/set-attr el attr (callback new-value)))))

(defn $ev [event callback]
  (fn [el owner]
    (dom/on el event callback)))

(defn <$
  "whenever the observable value changes, call (ctor new) and replace
   the previous node with the new one"
  [observable ctor]
  (reify
    IDynamicElement
    (-insert-element! [_ el]
      (let [current-el (atom nil)
            sub-key (gen-scope-key)
            sub-fn (fn sub-fn [new]
                     (let [old-el @current-el]
                       (when (and old-el (satisfies? IDestructable old-el))
                         (destroy! old-el))
                         
                       (let [new-def (ctor new)
                             ;; if no node needs to be constructed at this point
                             ;; just use empty text node as a placeholder
                             new-el (construct (if (nil? new-def) "" new-def))]

                         (reset! current-el new-el)

                         (if old-el
                           (dom/replace-node old-el new-el)
                           (dom/append el new-el))
                         )))]

        (subscribe! observable sub-key sub-fn)
        (sub-fn (-pull observable))
        nil
        ))))

(defn construct-scoped
  ([el]
     (construct-scoped *scope* el))
  ([parent el]
     (let [scope (new-scope parent)
           root (binding [*scope* scope]
                  (construct el))]
       (ScopedElement. scope root)
       )))

(defn case-el* [observable branches]
  (reify
    IDynamicElement
    (-insert-element! [_ el]
      (let [parent-scope *scope*

            current (atom nil)
            
            sub-key (gen-scope-key)

            sub-fn (fn sub-fn [new]
                     (let [old @current]
                       (when old
                         (destroy! old))
                         
                       (let [new-branch (branches new)
                             new-el (construct-scoped
                                     parent-scope
                                     (let [new-def (new-branch)]
                                       (if (nil? new-def) "" new-def)))]

                         (reset! current new-el)
                           
                         (if old
                           (dom/replace-node old new-el)
                           (dom/append el new-el))
                         )))]

        (subscribe! observable sub-key sub-fn)
        (sub-fn (-pull observable))
        nil
        ))))

(extend-protocol IElementList
  cljs.core/IndexedSeq
  (-get-elements [this] this)
  cljs.core/LazySeq
  (-get-elements [this] this))

(extend-protocol IObservable
  cljs.core/PersistentVector
  (-pull [[source key]]
    (get @source key))
  (-subscribe! [[source key] sub-key sub-fn]
    (add-watch source (.-key sub-key)
               (fn [_ _ old new]
                 (let [ov (get old key)
                       nv (get new key)]
                   (when-not (identical? ov nv)
                     (sub-fn nv))))))
  (-unsubscribe! [[source _] sub-key]
    (remove-watch source (.-key sub-key))))

(extend-protocol IConstructElement
  cljs.core/PersistentVector
  (construct [el]
    (let [[factory & children] el]
     (when-not (satisfies? IElementFactory factory)
       (throw (ex-info (str "invalid vector in tree, must start with factory, got " (type factory)) {:el el})))
     (-create-element factory children)))

  string
  (construct [el]
    (js/document.createTextNode el))
  
  number
  (construct [el]
    (js/document.createTextNode (str el))))

;; BACKWARDS COMPATIBILTY HACK, SHOULD NOT BE USED!
(extend-protocol IElementFactory
  cljs.core/Keyword
  (-create-element [kw children]
    (let [[tag id class] (dom/parse-tag kw)]
      (dom-element tag
                   (-> {}
                       (cond->
                        id (assoc :id id)
                        class (assoc :class class)))
                   []
                   children))))

(extend-protocol IDynamicElement
  cljs.core/PersistentArrayMap
  (-insert-element! [this el]
    (apply-attributes! el this))
  cljs.core/PersistentHashMap
  (-insert-element! [this el]
    (apply-attributes! el this)))

;; from https://github.com/swannodette/om/blob/master/src/om/dom.clj
(sm/define-node-factories
  [a
   abbr
   address
   area
   article
   aside
   audio
   b
   base
   bdi
   bdo
   big
   blockquote
   body
   br
   button
   canvas
   caption
   cite
   code
   col
   colgroup
   data
   datalist
   dd
   del
   dfn
   div
   dl
   dt
   em
   embed
   fieldset
   figcaption
   figure
   footer
   form
   h1
   h2
   h3
   h4
   h5
   h6
   head
   header
   hr
   html
   i
   iframe
   img
   ins
   kbd
   keygen
   label
   legend
   li
   link
   main
   map
   mark
   marquee
   menu
   menuitem
   meta
   meter
   nav
   noscript
   object
   ol
   optgroup
   output
   p
   param
   pre
   progress
   q
   rp
   rt
   ruby
   s
   samp
   script
   section
   select
   small
   source
   span
   strong
   style
   sub
   summary
   sup
   table
   tbody
   td
   tfoot
   th
   thead
   time
   title
   tr
   track
   u
   ul
   var
   video
   wbr
    
   ;; svg
   circle
   ellipse
   g
   line
   path
   polyline
   rect
   svg
   text
   defs
   linearGradient
   polygon
   radialGradient
   stop
   tspan])

(deftype ComponentInstance [spec scope state ^:mutable dom]
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
    state)

  dom/IElement
  (-to-dom [_] dom))

(defn fire-event! [obj ev & args]
  (log "fire-event!" obj ev args))

(defn mutual-destruction! [scope cmp]
  (log "mutual destruct" scope cmp))

(defn create-instance
  [spec attr init-fns children]
  (let [scope (new-scope *scope*)
        cmp (ComponentInstance. spec scope attr nil)
        
        _ (do (mutual-destruction! scope cmp) 
              (fire-event! cmp :init))

        dom-fn (:dom spec)
        dom (binding [*scope* scope]
              (construct (dom-fn cmp children)))]

    (set! (.-dom cmp) dom)

    (doseq [init-fn init-fns]
      (init-fn dom cmp))
    
    (fire-event! cmp :dom/init)

    cmp))

(defn optimize-spec [spec]
  spec)



