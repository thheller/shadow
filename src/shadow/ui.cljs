(ns shadow.ui
  (:require [shadow.object :as so]
            [shadow.keyboard :as kb]
            [shadow.dom :as dom]
            [goog.dom.forms :as gf]
            [clojure.string :as str]
            [cljs.reader :as reader]
            ))

(defn as-path [k]
  (if (keyword? k) [k] k))

;; behavior
(def update-on-change
  [:input/change (fn [obj attr value]
                   (so/update! obj assoc-in attr value))])

(defprotocol IInputType
  (-decode [this string] "decode from string to whatever")
  (-encode [this val] "encode from whatever to string to use in dom"))

(defn future-node
  "lazily create a dom node when attr first appears on object"
  [oref attr dom-key events-key placeholder]
  (let [node-atom (atom nil)
        attr (if (vector? attr) attr [attr])
        watch-id (gensym)
        placeholder-node (dom/build placeholder)]

    (add-watch oref watch-id (fn [_ _ _ new]
                               (when-let [value (get-in new attr)]
                                 (let [node (so/make-dom oref dom-key events-key value)]
                                   (dom/replace-node placeholder-node node))
                                 (remove-watch oref watch-id)
                                 )))

    placeholder-node
    ))

(def int-type
  (reify IInputType
    (-decode [this value]
      (when (re-find #"^\d+$" value)
        (js/parseInt value)))
    (-encode [this val]
      (str val))))

(def text-type
  (reify IInputType
    (-decode [this string]
       (str/trim string))
    (-encode [this val]
      (str val))))

(def keyword-type
  (reify IInputType
    (-decode [this string]
      (when (and (string? string)
                 (not= "" string))
        (keyword (.substring string 1))))
    (-encode [this val]
      (str val))))

;;; INPUT

(so/define-event :input/change "when an input is changed and its value was validated"
  [[:attr "the attr arg"]
   [:value "the new value"]
   [:input "the dom input?"]])

(so/define-event :input/blur "when the user leaves an input field, should follow :input/validated unlike :input/change will also fire when input wasnt changed"
  [[:attr "the attr arg"]
   [:value "the new value"]
   [:input "the dom input?"]])

(so/define-event :input/focus "when the user focuses an input"
  [[:attr "the attr arg"]
   [:input "the input"]])

(so/define-event :input/enter "when enter was pressed and the value validated"
  [[:attr "attr"]
   [:value "value"]
   [:input "input"]])

(so/define-event :input/validated "whenever the input was validated to be correct"
  [[:attr "attr eg. [:some :attr]"]
   [:value "the value"]
   [:dom-input "the dom node the value came from"]])

(so/define-event :input/error "whenever the input was validated to be falsy"
  [[:attr "attr eg. [:some :attr]"]
   [:msg "the error msg"]
   [:dom-input "the dom node the string value came from"]])

(so/define-event :input/force-validation "trigger a validation and thus resulting in input-validated/input-error"
  [])

(so/define-event :input/set-values "update the dom with new values"
  [[:new-values "a map containing the values to set, each input should check for its own attribute and only update if included in the map, otherwise ignore the event"]])

(so/define-event :input/set-options "update the dom with new options, probably only interesting for select boxes"
  [[:new-options "a map containing the options to set, each input should check for its own attribute and only update if included in the map, otherwise ignore the event"]])


(defn index-of [items check-fn]
  (loop [items items
         idx 0]
    (if (empty? items)
      nil
      (let [f (first items)]
        (if (check-fn f)
          idx
          (recur (rest items) (inc idx)))))))

(defn dom-name [attr]
  (str/join "." (map name attr)))

;; should be a macro at some point
(defn with-timeout [ms callback]
  (.setTimeout js/window callback ms))


;; http://zilence.net/sj.gif
(defn group-select-options
  "transforms a list of maps into [[group-key [[value-key label] [value-key label]]] ...]"
  [items group-key value-key label-fn]
  (vec (->> items
            (group-by group-key)
            (sort-by first)
            (map (fn [[group items]]
                   [group (vec (map (fn [item]
                                         [(get item value-key)
                                          (label-fn item)])
                                       items))])
                 ))))

(defn never-has-errors [obj field value]
  nil)

(defn get-error
  "get error message for value (nil if value is accepted)"
  [{:keys [a parent] :as input} value]
  (let [get-error-fn (so/get-type-attr parent :input/get-error never-has-errors)]
    (get-error-fn parent a value)
    ))

(defn do-validation
  "validate value and notify parent
   notifies obj with [:input/error attr error-msg input] when an error is found
   notifies obj with [:input/validated attr value input] when no error is found"
  [{:keys [a parent] :as input} value]
  (let [get-error-fn (so/get-type-attr parent :input/get-error never-has-errors)]
    (if-let [msg (get-error input value)]
      (do
        (so/notify! parent :input/error a msg input)
        false)
      (do
        (so/notify! parent :input/validated a value input)
        true))
      ))

(defn quick-validation [{:keys [input-type] :as this}]
  (let [sval (dom/get-value this)
        value (-decode input-type sval)]
    (do-validation this value)
    ))

(def dom-input-behavior
  [:input/force-validation quick-validation
   :input/set-values (fn [{:keys [a input-type] :as this} new-values]
                       (when-let [nv (get-in new-values a)]
                         (so/update! this assoc :v nv)
                         (dom/set-value this (-encode input-type nv))
                         ))])


(defn dom-select-options-grouped [input-type options]
  ;; FIXME: can you nest more than 1 lvl? never done it.
  (for [[group-label group-options] options]
    (if (string? group-options)
      ;; single option
      [:option {:value (-encode input-type group-label)} group-options]
      ;; optgroup
      [:optgroup {:label group-label}
       (for [[value label] group-options]
         [:option {:value (-encode input-type value)}
          (str label)])])))

(defn dom-select-options-flat [input-type options]
  (for [[value label] options
        :let [label (or label value)]]
    [:option {:value (-encode input-type value)} (str label)]))

(defn dom-select-options [{:keys [input-type group] :as this} options]
  (if group
    (dom-select-options-grouped input-type options)
    (dom-select-options-flat input-type options)
    ))

(so/define ::dom-select
  :dom (fn [{:keys [attrs options] :or {attrs {}} :as this}]
         [:select attrs (dom-select-options this options)])

  :behaviors [dom-input-behavior]

  :on [:dom/init (fn [{:keys [input-type v] :as this}]
                   (dom/set-value this (-encode input-type v)))

       :input/set-options (fn [{:keys [a input-type parent v] :as this} new-options]
                            (when-let [nv (get-in new-options a)]
                              (so/update! this assoc :options nv) ;; dont really need to do this?

                              (let [curval (-encode input-type v)]
                                (dom/reset this)
                                (doseq [opt (dom-select-options this nv)]
                                  (dom/append this opt))
                                
                                (let [values (dom/select-option-values this)
                                      value-set (set values)]
                                  (if (and curval (contains? value-set curval)) ;; new options include all value, reselect it
                                    (dom/set-value this curval)
                                    ;; new options dont include old one, select first option and notify parent
                                    ;; FIXME: what to do when options are empty? really more of an app concern?
                                    (when-let [n-val (first values)]
                                      (dom/set-value this n-val)
                                      (so/notify! parent :input/change a (-decode input-type n-val) this)))
                                  ))))]

  :dom/events [:change  (fn [{:keys [a parent input-type] :as this} ev]
                          (let [sval (dom/get-value this)
                                value (-decode input-type sval)]
                            (when (do-validation this value)
                              (so/notify! parent :input/change a value this)))
                          )])

(defn dom-select
  ([obj attr type options]
     (dom-select obj attr type options {}))
  ([obj attr type options select-attrs]
     (when-not (satisfies? IInputType type)
       (throw (ex-info "dom select type must support protocol InputType" {:type type})))

     (when-not (vector? options)
         (throw (ex-info "select options should be a vector" {:parent obj :attr attr :options options})))

     ;; options should be [[value "label"] [value "label"]

     (let [a (as-path attr)
           v (get-in obj a)]

       (so/create ::dom-select
                  {:parent obj
                   :attrs (dissoc select-attrs :group)
                   :options options
                   :group (:group select-attrs)
                   :a a
                   :v v
                   :input-type type}))))

(defn dom-select-grouped
  ([obj attr type options]
     (dom-select obj attr type options {:group true}))
  ([obj attr type options select-attrs]
     (dom-select obj attr type options (assoc select-attrs :group true))))

(defn process-dom-input [{:keys [parent a input-type] :as this} ev-type]
  (let [sval (dom/get-value this)
        value (-decode input-type sval)]
    (when (do-validation this value)
      (so/notify! parent ev-type a value this))))

(so/define ::dom-input
  :dom (fn [{:keys [attrs] :as this}]
         [:input (merge {:type "text"} attrs)])

  :behaviors [dom-input-behavior]

  :on [:dom/init (fn [{:keys [a v input-type] :as this}]
                   (when v
                     (dom/set-value this (-encode input-type v))))

       ;; there is no API to figure out if a field was filled by autocomplete
       ;; unless a value has been given previously, just take a peek
       ;; the delay is required since the browser does not fill the value when the element enters the dom
       ;; but does after some delay, sometimes even 250 is not enough, depends on what else is going on
       :dom/entered (fn [{:keys [parent a v input-type] :as this}]
                      (when-not (= "off" (dom/attr this :autocomplete)) ;; fucking autocomplete, we need a standard for this stuff
                        (when (= v "")
                          (.setTimeout
                           js/window
                           (fn []
                             (let [sv (dom/get-value this)]
                               (when (not= sv "")
                                 (let [new-value (-decode input-type sv)]
                                   (when (do-validation this new-value)
                                     (so/log "found autocomplete field" parent a)
                                     (so/notify! parent :input/change a new-value this)
                                     )))))
                           250
                           ))))

       :dom/init (fn [{:keys [a parent input-type capture] :as this}]
                   (when (contains? capture :enter)
                     (dom/on this :keyup
                             (fn [e]
                               (when (= 13 (.-keyCode e))
                                 (let [sval (dom/get-value this)
                                       new-value (-decode input-type sval)]

                                   ;; FIXME: need to figure out what the best behavior is here, mostly related to validations
                                   ;; not using change event since I basically want validation on blur
                                   ;; doing it again on change is kinda pointless since we can do it here
                                   (when (do-validation this new-value)
                                     (so/notify! parent :input/enter a new-value this)
                                     )))))))]

  :dom/events [:focus (fn [{:keys [a parent] :as this} ev]
                        (so/notify! parent :input/focus a this))
               
               :change (fn [{:keys [parent a input-type v] :as this} ev]
                         (let [sval (dom/get-value this)
                               new-value (-decode input-type sval)]

                           (when (do-validation this new-value)
                             (when (not= new-value v)
                               (so/update! this assoc :v new-value)
                               (so/notify! parent :input/change a new-value this))
                             )))

               :blur (fn [{:keys [parent a input-type v] :as this} ev]
                       (so/notify! parent :input/blur a this)

                       (let [sval (dom/get-value this)
                             new-value (-decode input-type sval)]

                         ;; FIXME: validates once on change and once on blur
                         (do-validation this new-value)))])

(defn dom-input [obj attr type attrs]
  (when-not (satisfies? IInputType type)
    (throw (ex-info "dom input type must support protocol InputType" {:type type})))

  (let [attr (as-path attr)
        capture (:capture attrs #{:change})
        init-val (or (get-in obj attr) (:default attrs ""))

        input-attrs (dissoc attrs :capture :default)
        input-attrs (merge {:name (dom-name attr)} ;; automated naming may lead to conflicts
                           input-attrs)]

    (so/create ::dom-input {:parent obj
                            :attrs input-attrs
                            :capture capture
                            :input-type type
                            :a attr
                            :v init-val})))


(so/define ::dom-checkbox
  :dom (fn [{:keys [attrs] :as this}]
         [:input attrs])

  :on [:input/force-validation (fn [this]
                                 (do-validation this (:v this))) 

       :input/set-values (fn [{:keys [a negated] :as this} new-values]
                           (let [nv (get-in new-values a)]
                             (when-not (nil? nv)
                               (so/update! this assoc :v nv)
                               (let [dv (if negated (not nv) nv)]
                                 (dom/check this dv)))
                             ))]

  :dom/events [:change (fn [{:keys [a negated parent] :as this} e]
                         (let [nv (dom/checked? this)
                               nv (if negated (not nv) nv)]
                           (so/update! this assoc :v nv)
                           (when (do-validation this nv)
                             (so/notify! parent :input/change a nv this)) 
                           ))])

(defn dom-checkbox
  ([obj attr]
     (dom-checkbox obj attr {}))
  ([obj attr attrs]
     (let [a (as-path attr)
           v (get-in obj a false)
           negated (:negated attrs false)
           v (if negated
               (not v)
               v)
           attrs (dissoc attrs :negated)
           attrs (assoc attrs :type "checkbox")
           attrs (if v
                   (assoc attrs :checked true)
                   attrs)]

       (so/create ::dom-checkbox {:parent obj
                                  :negated negated
                                  :a a
                                  :v v
                                  :attrs attrs})
       )))

(so/define ::dom-radio
  ;; [:input {:type "radio" :checked true}] doesnt work for some reason
  ;; dom pretends to be checked but isn't displayed as such
  ;; no idea why its behaving this way, only works when selected WITH timeout
  ;; otherwise its not displayed as checked

  :dom (fn [{:keys [attrs] :as this}]
         [:input (dissoc attrs :checked)])

  :on [:dom/init (fn [{:keys [attrs] :as this}]
                   (when (:checked attrs)
                     (with-timeout 1 #(dom/check this true)) 
                     ))

       :input/force-validation (fn [this]
                                 (do-validation this (:v this))) 

       :input/set-values (fn [{:keys [a v negated] :as this} new-values]
                           (let [nv (get-in new-values a)]
                             (dom/check this (= nv v))
                             ))]

  :dom/events [:change (fn [{:keys [a v parent] :as this} e]
                         (when (dom/checked? this)
                           (when (do-validation this v)
                             (so/notify! parent :input/change a v this))) 
                         )])

(defn dom-radio
  ([obj attr input-type v]
     (dom-radio obj attr input-type v {}))
  ([obj attr input-type v attrs]
     (let [a (as-path attr)
           cv (get-in obj a false)
           attrs (assoc attrs
                   :type "radio"
                   :name (dom-name a))
           attrs (if (= cv v)
                   (assoc attrs :checked true)
                   attrs)]
       
       (so/log "dom-radio" a v cv attrs)

       (so/create ::dom-radio {:parent obj
                               :a a
                               :v v
                               :attrs attrs}))))

(so/define ::dom-textarea
  :dom (fn [{:keys [attrs v] :as this}]
         [:textarea attrs v])

  :behaviors [dom-input-behavior]

  :dom/events [:change (fn [{:keys [a parent input-type] :as this} e]
                         (let [sval (dom/get-value this)
                               value (-decode input-type sval)]
                           (when (do-validation this value)
                             (so/notify! parent :input/change a value this))))])

(defn dom-textarea
  [obj attr type attrs]
  (when-not (satisfies? IInputType type)
    (throw (ex-info "dom input type must support protocol InputType" {:type type :attr attr :attrs attrs})))

  (let [a (as-path attr)
        v (get-in obj a "")]
    (so/log "dom-textarea" a v)
    (so/create ::dom-textarea {:parent obj
                               :a a
                               :v v
                               :input-type type
                               :attrs attrs
                               })))

(def timeouts (atom {}))

(defn timeout [delay callback]
  (.setTimeout js/window callback delay))

(defn keyed-timeout
  ([key callback] (keyed-timeout key callback 3000))
  ([key callback time-ms]
     (let [cur (get @timeouts key)]
       (when cur
         (.clearTimeout js/window cur))
       (let [timeout-fn (fn []
                          (swap! timeouts dissoc key)
                          (callback))
             timeout-id (.setTimeout js/window timeout-fn time-ms)]
         (swap! timeouts assoc key timeout-id)
         ))))

(def local-storage (.-localStorage js/window))

;; (def my-atom (ui/store-locally (atom defaults) "key"))
;; (def my-atom (atom defaults)
;; (ui/store-locally my-atom "key")

(defn store-locally [atm name]
  (let [stored-value (aget local-storage name)]

    (when stored-value
      (reset! atm (reader/read-string stored-value)))

    (add-watch atm :store-locally
               (fn [_ _ _ new]
                 (aset local-storage name (pr-str new))
                 ))

    atm
    ))



(defn distinct-by
  "Returns a lazy sequence of the (ex elements) of coll with duplicates removed"
  {:added "1.0"
   :static true
   :from "clojure.core/distinct"}
  [ex coll]
  (let [step (fn step [xs seen]
               (lazy-seq
                ((fn [[f :as xs] seen]
                   (let [fex (ex f)]
                     (when-let [s (seq xs)]
                       (if (contains? seen fex)
                         (recur (rest s) seen)
                         (cons f (step (rest s) (conj seen fex)))))))
                 xs seen)))]
    (step coll #{})))

(defn self-destruct
  "behavior to self destruct objects on click or timeout

   useful for notifications and such
   
   Example:
   (so/define ::my-object
     :dom (fn ...)
     :behaviors [(ui/self-destruct 3000)])
  
  this will so/destroy! (the default) the object 3000ms after it entered the dom"
  ([timeout]
     (self-destruct timeout so/destroy!))
  ([timeout destruct-fn]
     [:dom/entered
      (fn [this]
        (let [timer (ui/with-timeout timeout
                      #(destruct-fn this))]
          (dom/on this :click
                  (fn [e]
                    (.clearTimeout js/window timer)
                    (destruct-fn this)))))]))
