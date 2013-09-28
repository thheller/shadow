(ns shadow.dom
  (:refer-clojure :exclude [remove contains?])
  (:require [goog.dom :as dom]
            [goog.dom.forms :as gf]
            [goog.dom.classlist :as gcls]
            [goog.style :as gs]
            [goog.style.transition :as gst]
            [goog.string :as gstr]
            [clojure.string :as str]))

(def transition-supported? (gst/isSupported))

(defprotocol IElement
  (-to-dom [this]))

(defn- lazy-native-coll-seq [coll idx]
  (when (< idx (.-length coll))
    (lazy-seq (cons (aget coll idx)
                    (lazy-native-coll-seq coll (inc idx))))
    ))

(deftype NativeColl [coll]
  IDeref
  (-deref [this] coll)

  IIndexed
  (-nth [this n] (aget coll n))
  (-nth [this n not-found] (or (aget coll n) not-found))

  ICounted
  (-count [this] (.-length coll))

  ISeqable
  (-seq [this] (lazy-native-coll-seq coll 0))

  IElement
  (-to-dom [this] coll)
  )

(defn dom-node [el]
  ;; FIXME: this method is called alot, how expensive is this check?
  ;; protocols on native elements are funky
  (if (satisfies? IElement el)
    (-to-dom el)
    el))

(def build dom-node)

(defn ev-stop [e]
  (if (.-stopPropagation e)
    (do
      (.stopPropagation e)
      (.preventDefault e))
    (do
      (set! (.-cancelBubble e) true)
      (set! (.-returnValue e) false)))
  true)

(defn contains?
  "check wether a parent node (or the document) contains the child"
  ([el]
     (dom/contains js/document (dom-node el)))
  ([parent el]
     (dom/contains (dom-node parent) (dom-node el))))


(defn add-class [el cls]
  (gcls/add (dom-node el) cls))

(defn remove-class [el cls]
  (gcls/remove (dom-node el) cls))

(defn toggle-class
  ([el cls]
     (gcls/toggle (dom-node el) cls))
  ([el cls v]
     (if v
       (add-class el cls)
       (remove-class el cls))))


(defn has-class? [el cls]
  (gcls/contains (dom-node el) cls))

(defn- merge-class-string [current extra-class]
  (if (seq current)
    (str current " " extra-class)
    extra-class))


;; before strange code!!!
;; just because I can and messing with a huge regexp sucks
;; messing with a native javascript object should also be faster
(defn parse-tag [spec]
  (let [spec (name spec)
        fdot (.indexOf spec ".")
        fhash (.indexOf spec "#")]
    (cond
     (and (= -1 fdot) (= -1 fhash))
     [spec nil nil]

     (= -1 fhash)
     [(.substring spec 0 fdot)
      nil
      (str/replace (.substring spec (inc fdot)) #"\." " ")]

     (= -1 fdot)
     [(.substring spec 0 fhash)
      (.substring spec (inc fhash))
      nil]

     (> fhash fdot)
     (throw (str "cant have id after class?" spec))

     :else
     [(.substring spec 0 fhash)
      (.substring spec (inc fhash) fdot)
      (str/replace (.substring spec (inc fdot)) #"\." " ")])))


(defn create-dom-node [tag-def props]
  (let [[tag-name tag-id tag-classes] (parse-tag tag-def)]
    (when tag-id
      (aset props "id" tag-id))

    (when tag-classes
      (aset props "class" (merge-class-string (aget props "class") tag-classes)))

    (dom/createDom tag-name props)
    ))

(defn destructure-node [[nn np & nc :as node]]
  (when-not (keyword? nn)
    (throw (ex-info "invalid dom node" {:node node})))

  (cond
   (and (nil? np) (nil? nc)) ;; [:div.something]
   [(create-dom-node nn (js-obj)) '()]
   (map? np) ;; [:div.something {:some "attr"}]
   [(create-dom-node nn (clj->js np)) nc]
   :else ;; [:div.something "content" "more-content"]
   [(create-dom-node nn (js-obj)) (conj nc np)]))


;; restore sanity!

(defn macro-node [tag-name tag-id tag-classes attrs & body]
  (let [[attrs body] (if (map? attrs)
                       [attrs body]
                       [{} (cons attrs body)])
        props (clj->js attrs)]

    (when tag-id
      (aset props "id" tag-id))

    (when tag-classes
      (aset props "class" (merge-class-string (aget props "class") tag-classes)))

    (let [node (dom/createDom tag-name props)]
      (doseq [child-struct body]
        (if (seq? child-struct)
          (doseq [child child-struct]
            (dom/append node (dom-node child)))
          (dom/append node (dom-node child-struct))
          ))
      node
      )))

(defn make-dom-node [structure]
  (let [[node node-children] (destructure-node structure)]

    (doseq [child-struct node-children]
      (let [children (dom-node child-struct)]
        (if (seq? children)
          (doseq [child children]
            (when child
              (dom/append node child)))
          (do
            (dom/append node children)))))
    node))

(extend-protocol IElement
  string
  (-to-dom [this]
    (if (keyword? this)
      (make-dom-node [this])
      (.createTextNode js/document this)))
  
  ;;Keyword
  ;; (-to-dom [this] (make-dom-node [this]))

  number
  (-to-dom [this]
    (.createTextNode js/document (str this)))

  PersistentVector
  (-to-dom [this]
    (make-dom-node this))

  LazySeq
  (-to-dom [this]
    (map -to-dom this))

  js/Text
  (-to-dom [this] this)

  nil
  (-to-dom [_] nil)
  )

(when (js* "((typeof HTMLElement) != 'undefined')")
  (extend-protocol IElement
    js/HTMLElement
    (-to-dom [this] this)
    ))

(when (js* "((typeof DocumentFragment) != 'undefined')")
  (extend-protocol IElement
    js/DocumentFragment
    (-to-dom [this] this)
    ))


(defn append
  ([node] (dom/append (.-body js/document) (dom-node node)))
  ([el node] (dom/append (dom-node el) (dom-node node))))

(defn query-one
  ([sel] (.querySelector js/document sel))
  ([sel root] (.querySelector (dom-node root) sel)))

(defn query
  ([sel] (NativeColl. (.querySelectorAll js/document sel)))
  ([sel root] (NativeColl. (.querySelectorAll (dom-node root) sel))))

(def dom-listen (if (.-addEventListener js/document)
                  (fn dom-listen-good [el ev handler]
                    (.addEventListener el
                                       ev
                                       (fn [e] (handler e el))
                                       false ;; ancient browsers want 3 args
                                       ))
                  (fn dom-listen-ie [el ev handler]
                    (try
                      (.attachEvent el (str "on" ev) (fn [e] (handler e el)))
                      (catch js/Object e
                        (.log js/console "didnt support attachEvent" el e)))
                    )))

(defn on-query [root-el ev selector handler]
  (doseq [el (query selector root-el)]
    (dom-listen el (name ev) handler)
    ))

(defn on
  ([el ev handler]
     (on el ev handler false))
  ([el ev handler capture]
     (if (vector? ev)
       (on-query el (first ev) (second ev) handler)
       (dom-listen (dom-node el) (name ev) handler))))

(defn by-id
  ([id el] (.getElementById (dom-node el) id))
  ([id] (.getElementById js/document id)))

(defn reset
  "clear node children"
  [node]
  (dom/removeChildren (dom-node node)))

(defn remove [node]
  (if (satisfies? ISeqable node)
    (doseq [n node] (remove n))
    (dom/removeNode node)))

(defn replace-node [old new]
  ;; wth reverse
  (dom/replaceNode (dom-node new)
                   (dom-node old)))

(defn text
  ([el new-text] (set! (.-innerText (dom-node el)) new-text))
  ([el] (.-innerText (dom-node el))))

(defn check
  ([el] (check el true))
  ([el checked]
     (set! (.-checked (dom-node el)) checked)
     ))

(defn checked? [el] (.-checked (dom-node el)))

(defn form-elements [el]
  (NativeColl. (.-elements (dom-node el))))

(defn children [el]
  (NativeColl. (.-children (dom-node el))))

(defn attr
  ([el key] (.getAttribute (dom-node el) (name key)))
  ([el key default] (or (.getAttribute (dom-node el) (name key)) default)))

(defn set-attr [el key value]
  (dom/setProperties (dom-node el) (clj->js {key value})))

(defn del-attr [el key]
  (.removeAttribute (dom-node el) (name key)))

;; dont ever include a script including this in <head>!
(def data
  (if (.. js/document -body -dataset)
    (fn data-dataset [el key]
      (aget (-> el dom-node .-dataset) (gstr/toCamelCase (name key))))
    (fn data-get-attribute [el key]
      (.getAttribute (dom-node el) (str "data-" (name key)))) ;; fallback
    ))

(def set-data 
  (if (.. js/document -body -dataset)
    (fn set-data-dataset [el key value]
      (aset (-> el dom-node .-dataset) (gstr/toCamelCase (name key)) (str value)))
    (fn set-data-set-attribute [el key value]
      (.setAttribute (dom-node el) (str "data-" (name key)) (str value)))
    ))

(defn set-html [node text]
  (set! (.-innerHTML (dom-node node)) text))

(defn get-html [node]
  (.-innerHTML (dom-node node)))

(defn fragment [& nodes]
  (let [fragment (.createDocumentFragment js/document)]
    (doseq [node nodes]
      (.appendChild fragment (-to-dom node)))
    (NativeColl. fragment)
    ))


(defn eval-scripts
  "given a html string, eval all <script> tags and return the html without the scripts
   don't do this for everything, only content you trust."
  [s]
  (let [scripts (re-seq #"<script[^>]*?>(.+?)</script>" s)]

    (doseq [[script-tag script-body] scripts]
      (js/eval script-body))

    (reduce (fn [s [script-tag script-body]]
              (str/replace s script-tag ""))
            s
            scripts)
    ))

(defn str->fragment [s]
  (NativeColl. (dom/htmlToDocumentFragment s)))

(defn ancestor-by-class [el cls]
  (dom/getAncestorByClass (dom-node el) cls))

(defn ancestor-by-tag
  ([el tag] (dom/getAncestorByTagNameAndClass (dom-node el) (name tag)))
  ([el tag cls] (dom/getAncestorByTagNameAndClass (dom-node el) (name tag) (name cls))))

(defn get-value [dom]
  (gf/getValue (dom-node dom)))

(defn set-value [dom value]
  (gf/setValue (dom-node dom) value))

(defn set-style [el styles]
  (gs/setStyle (dom-node el) (clj->js styles)))

(defn get-position [el]
  (let [pos (gs/getClientPosition (dom-node el))]
    {:x (.-x pos) :y (.-y pos)}))

(defrecord Size [w h])
(defn size->clj [size]
  (Size. (.-width size) (.-height size)))

(defn get-size [el]
  (size->clj (gs/getSize (dom-node el))))

(defn get-height [el]
  (-> el get-size :h))

(defn get-viewport-size []
  (size->clj (dom/getViewportSize)))

(defn first-child [el]
  (aget (.-children (dom-node el)) 0))

(defn select-option-values [el]
  (let [native (dom-node el)
        opts (aget native "options")]
    (areduce opts i ret []
             (conj ret (aget opts i "value")))
    ))

(defn build-url [path query-params]
  (if (empty? query-params)
    path
    (str path "?" (str/join "&" (map (fn [[k v]]
                                       (str (name k) "=" (js/encodeURIComponent (str v))))
                                     query-params)))
    ))
(defn redirect
  ([path]
     (redirect path {}))
  ([path query-params]
     (aset js/document "location" "href" (build-url path query-params))
     ))


(defn tag-name [el]
  (let [dom (dom-node el)]
    (.-tagName dom)))

(defn insert-after [ref new]
  (dom/insertSiblingAfter (dom-node new)
                          (dom-node ref)))

(defn insert-before [ref new]
  (dom/insertSiblingBefore (dom-node new)
                           (dom-node ref)))

(defn get-parent [el]
  (dom/getParentElement (dom-node el)))


(defn get-next-sibling [el]
  (dom/getNextElementSibling (dom-node el)))

(defn get-previous-sibling [el]
  (dom/getPreviousElementSibling (dom-node el)))
