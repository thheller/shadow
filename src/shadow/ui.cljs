(ns shadow.ui
  (:require [shadow.object :as obj]
            [shadow.keyboard :as kb]
            [shadow.dom :as dom]
            [goog.dom.forms :as gf]
            [clojure.string :as str]
            [cljs.reader :as reader]
            ))


(defprotocol IInputType
  (-dom-type [this])
  (-validate [this string])
  (-encode [this val]))

(defn future-node
  "lazily create a dom node when attr first appears on object"
  [oref attr dom-key events-key placeholder]
  (let [node-atom (atom nil)
        attr (if (vector? attr) attr [attr])
        watch-id (gensym)
        placeholder-node (dom/build placeholder)]

    (add-watch oref watch-id (fn [_ _ _ new]
                               (when-let [value (get-in new attr)]
                                 (let [node (obj/make-dom oref dom-key events-key value)]
                                   (dom/replace-node placeholder-node node))
                                 (remove-watch oref watch-id)
                                 )))

    placeholder-node
    ))

(defn int-type []
  (reify IInputType
    (-dom-type [this] "number")
    (-validate [this value] {:valid true :value (js/parseInt value)})
    (-encode [this val]
      (str val))))

(defn text-type
  ([] (text-type (fn [val] {:valid true :value val})))
  ([validate-with]
     (reify IInputType
       (-dom-type [this] "text")
       (-validate [this string]
         (validate-with string))
       (-encode [this val]
         (str val)))))

(defn keyword-type []
  (reify IInputType
    (-dom-type [this] "text")
    (-validate [this string]
      {:valid true :value (keyword string)})
    (-encode [this val]
      (str val))))

;;; INPUT

(obj/define-event :input-change "when a validated input is changed"
  [[:attr "the attr arg"]
   [:value "the new value"]
   [:input "the dom input?"]])

(obj/define-event :input-enter "when enter was pressed and the value validated"
  [[:attr "attr"]
   [:value "value"]
   [:input "input"]])

(obj/define-event :input-validated "whenever the input was validated"
  [[:attr "attr"]
   [:valid "boolean"]
   [:sval "the string value entered"]
   [:error "the validation error"]])


(defn index-of [items check-fn]
  (loop [items items
         idx 0]
    (if (empty? items)
      nil
      (let [f (first items)]
        (if (check-fn f)
          idx
          (recur (rest items) (inc idx)))))))

(defn dom-select-grouped [obj attr type options-key select-attrs]
  (when-not (satisfies? IInputType type)
    (throw (ex-info "dom select type must support protocol InputType" {:type type})))

  ;; options-key should point to options in obj (I may want to bind to them!)
  ;; options should be [["group label" [[value "label"]
  ;;                                    [value "label"]]]

  (let [path (if (vector? attr) attr [attr])
        init-val (get-in obj path)
        options (get obj options-key)

        _ (when-not (vector? options)
            (throw (ex-info "select options should be a vector" {:options options})))

        select (dom/build [:select select-attrs
                           (for [[group-label group-options] options]
                             [:optgroup {:label group-label}
                              (for [[value label] group-options]
                                [:option {:value (-encode type value)
                                          :selected (when (= init-val value) true)}
                                 (str label)])])
                           ])]

    (dom/on select :change
            (fn [ev]
              (let [sv (dom/get-value select)
                    {:keys [valid value error]} (-validate type sv)]
                (obj/notify! obj :input-validated attr valid value error)
                (if valid
                  (obj/notify! obj :input-change attr value select)
                  (obj/log "dom-select-grouped with invalid value?" obj attr value select)
                  ))))

    select
    ))

(defn dom-select [obj attr type options-key select-attrs]
  (when-not (satisfies? IInputType type)
    (throw (ex-info "dom select type must support protocol InputType" {:type type})))

  ;; options-key should point to options in obj (I may want to bind to them!)
  ;; options should be [[value "label"] [value "label"]

  (let [path (if (vector? attr) attr [attr])
        init-val (get-in obj path)
        multiple (:multiple select-attrs)

        options (get obj options-key)

        _ (when-not (vector? options)
            (throw (ex-info "select options should be a vector" {:options options})))

        select (dom/build [:select select-attrs
                           (for [[_ label] options]
                             [:option (str label)])
                           ])]

    (when-let [init-idx (index-of options #(= init-val (first %)))]
      (set! (.-selectedIndex select) init-idx))

    (dom/on select :change
            (fn [ev]
              (let [si (.-selectedIndex select)
                    sv (first (nth options si))]
                (obj/notify! obj :input-change attr sv select))
              ))

    select
    ))



(defn dom-input [obj attr type attrs]
  (when-not (satisfies? IInputType type)
    (throw (ex-info "dom input type must support protocol InputType" {:type type})))

  (let [path (if (vector? attr) attr [attr])
        capture (:capture attrs #{:change})
        init-val (get-in obj path)
        init-sval (:value attrs)
        init-sval (if (nil? init-val)
                    ""
                    (-encode type init-val))

        input (dom/build [:input (-> attrs
                                     (dissoc :capture :bind)
                                     (assoc :value init-sval
                                            :type (-dom-type type)))])

        process-input (fn [notif]
                        (let [sval (dom/get-value input)
                              {:keys [valid error value]} (-validate type sval)]
                          (obj/notify! obj :input-validated attr valid sval error)
                          (when valid
                            (obj/notify! obj notif attr value input)))
                        )]

    (when (:bind attrs)
      (obj/bind-change obj path
                       (fn [old new]
                         (dom/set-value input (-encode type new))
                         )))

    (when (contains? capture :enter)
      (dom/on input :keyup
              (fn [ev]
                (when (= 13 (.-keyCode ev))
                  (dom/ev-stop ev)
                  (process-input :input-enter)
                  ))))

    (when (contains? capture :change)
      (dom/on input :change #(process-input :input-change)))

    input
    ))


(def timeouts (atom {}))

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






