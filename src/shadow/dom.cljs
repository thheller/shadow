(ns shadow.dom
  (:refer-clojure :exclude [remove])
  (:require [goog.dom :as dom]
            [goog.dom.forms :as gf]
            [goog.dom.classes :as gcls]
            [goog.style :as gs]
            [clojure.string :as str]))

(defprotocol IElement
  (-to-dom [this]))

(defn ev-stop [e]
  (.stopPropagation e)
  (.preventDefault e))

(defn add-class [el cls]
  (gcls/add (-to-dom el) cls))

(defn remove-class [el cls]
  (gcls/remove (-to-dom el) cls))

(declare build)

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
      (set! (.-id props) tag-id))

    (when tag-classes
      (aset props "class" (merge-class-string (aget props "class") tag-classes)))

    (dom/createDom tag-name props)
    ))

(defn destructure-node [[nn np & nc]]
  (cond
   (and (nil? np) (nil? nc)) ;; [:div.something]
   [(create-dom-node nn (js-obj)) '()]
   (map? np) ;; [:div.something {:some "attr"}]
   [(create-dom-node nn (clj->js np)) nc]
   :else ;; [:div.something "content" "more-content"]
   [(create-dom-node nn (js-obj)) (conj nc np)]))


;; restore sanity!

(defn make-dom-node [structure]
  (let [[node node-children] (destructure-node structure)]

    (doseq [child-struct node-children]
      (let [children (build child-struct)]
        (if (seq? children)
          (doseq [child children]
            (dom/append node child))
          (do
            (dom/append node children)))))
    node))

(defn build-seq [s]
  (map build s))

(extend-protocol IElement
  string
  (-to-dom [this]
    (if (keyword? this)
      (make-dom-node [this])
      (.createTextNode js/document this)))

  number
  (-to-dom [this]
    (.createTextNode js/document (str this)))

  js/Text
  (-to-dom [this] this)

  js/HTMLElement
  (-to-dom [this] this)

  PersistentVector
  (-to-dom [this]
    (make-dom-node this))

  LazySeq
  (-to-dom [this]
    (map -to-dom this))
  )

(defn build [struct]
  (let [dom (-to-dom struct)]
    dom))

(defn append
  ([node] (dom/append (.-body js/document) (-to-dom node)))
  ([el node] (dom/append (-to-dom el) (-to-dom node))))

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

  )

(comment ;; IE cannot dispatch protocols on "native" types apparently
  (defn nodelist->vector [nl]
    (js* "function(x) { var y = []; for (i = 0; i < x.length; i++) { y.push(x[i]); }; return cljs.core.PersistentVector.fromArray(y); }(~{nl});"))

  (when (js* "(typeof NodeList != 'undefined')")
    (extend-protocol clojure.core/ISeqable
      js/NodeList
      (-seq [this] (seq (nodelist->vector this)))))

  (when (js* "(typeof HTMLCollection != 'undefined')")
    (extend-protocol clojure.core/ISeqable
      js/HTMLCollection
      (-seq [this] (seq (nodelist->vector this))))))


(defn query-one
  ([sel] (.querySelector js/document sel))
  ([sel root] (.querySelector (-to-dom root) sel)))

(defn query
  ([sel] (NativeColl. (.querySelectorAll js/document sel)))
  ([sel root] (NativeColl. (.querySelectorAll (-to-dom root) sel))))

(defn- dom-listen [el ev handler]
  (.addEventListener el ev handler))

(defn on-query [root-el ev selector handler]
  (doseq [el (query selector root-el)]
    (dom-listen el (name ev) handler)
    ))

(defn get-node [el]
  (-to-dom el))

(defn on
  ([el ev handler]
     (on el ev handler false))
  ([el ev handler capture]
     (if (vector? ev)
       (on-query el (first ev) (second ev) handler)
       (dom-listen el (name ev) handler))))

(defn extract-ref [result node]
  (if-let [ref (.getAttribute node "data-ref")]
    (assoc result (keyword ref) node)
    result))

(deftype DomView [root refs]
  IElement
  (-to-dom [this] root)
  ILookup
  (-lookup [this k] (get refs k))
  (-lookup [this k d] (get refs k d))
  )

(defn view [structure]
  (let [node (build structure)
        node-map (reduce extract-ref {} (query "[data-ref]" node))
        node-map (extract-ref node-map node)]
    (DomView. node node-map)))

(defn by-id
  ([id el] (.getElementById (-to-dom el) id))
  ([id] (.getElementById js/document id)))

(defn reset
  "clear node children"
  [node]
  (dom/removeChildren node))

(defn remove [node]
  (if (satisfies? ISeqable node)
    (doseq [n node] (remove n))
    (dom/removeNode node)))

(defn replace-node [old new]
  ;; wth reverse
  (dom/replaceNode (-to-dom new)
                   (-to-dom old)))

(defn text
  ([el new-text] (set! (.-innerText (-to-dom el)) new-text))
  ([el] (.-innerText (-to-dom el))))

(defn check
  ([el] (check el true))
  ([el checked]
     (set! (.-checked (-to-dom el)) checked)
     ))

(defn checked? [el] (.-checked (-to-dom el)))

(defn form-elements [el]
  (NativeColl. (.-elements (-to-dom el))))

(defn children [el]
  (NativeColl. (.-children (-to-dom el))))

(defn attr
  ([el key] (.getAttribute (-to-dom el) (name key)))
  ([el key default] (or (.getAttribute (-to-dom el) (name key)) default)))

;; dont ever include a script including this in <head>!
(def data (if (.. js/document -body -dataset)
            (fn data [el key] (aget (-> el -to-dom .-dataset) (name key)))
            (fn data [el key] (attr el key)) ;; fallback
            ))

(defn set-attr [el key value]
  (dom/setProperties (-to-dom el) (clj->js {key value})))

(defn ancestor-by-class [el cls]
  (dom/getAncestorByClass (-to-dom el) cls))

(defn ancestor-by-tag
  ([el tag] (dom/getAncestorByTagNameAndClass (-to-dom el) (name tag)))
  ([el tag cls] (dom/getAncestorByTagNameAndClass (-to-dom el) (name tag) (name cls))))

(defn get-value [dom]
  (gf/getValue (-to-dom dom)))

(defn set-value [dom value]
  (gf/setValue (-to-dom dom) value))

(defn set-style [el styles]
  (gs/setStyle (-to-dom el) (clj->js styles)))

(defn get-position [el]
  (let [pos (gs/getPosition (-to-dom el))]
    {:x (.-x pos) :y (.-y pos)}))

(defn get-size [el]
  (let [size (gs/getSize (-to-dom el))]
    {:w  (.-width size) :h (.-height size)}
    ))


