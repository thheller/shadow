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
  [:input-change (fn [obj attr value]
                   (so/update! obj assoc-in attr value))])

(defprotocol IInputType
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
                                 (let [node (so/make-dom oref dom-key events-key value)]
                                   (dom/replace-node placeholder-node node))
                                 (remove-watch oref watch-id)
                                 )))

    placeholder-node
    ))

(defn int-type []
  (reify IInputType
    (-validate [this value]
      (if (re-find #"^\d+$" value)
        {:valid true :value (js/parseInt value)}
        {:valid false :error "Ungültige Zahl."}))
    (-encode [this val]
      (str val))))

(defn text-type
  ([] (text-type (fn [val] {:valid true :value val})))
  ([validate-with]
     (reify IInputType
       (-validate [this string]
         (validate-with string))
       (-encode [this val]
         (str val)))))

(defn keyword-type []
  (reify IInputType
    (-validate [this string]
      {:valid true :value (keyword (.substring string 1))})
    (-encode [this val]
      (str val))))

;;; INPUT

(so/define-event :input-change "when a validated input is changed"
  [[:attr "the attr arg"]
   [:value "the new value"]
   [:input "the dom input?"]])

(so/define-event :input-enter "when enter was pressed and the value validated"
  [[:attr "attr"]
   [:value "value"]
   [:input "input"]])

(so/define-event :input-validated "whenever the input was validated"
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

(defn dom-select-grouped
  ([obj attr type options-key]
     (dom-select-grouped obj attr type options-key {}))
  ([obj attr type options-key select-attrs]
      (when-not (satisfies? IInputType type)
        (throw (ex-info "dom select type must support protocol InputType" {:type type})))

      ;; options-key should point to options in obj (I may want to bind to them!)
      ;; options should be [["group label" [[value "label"]
      ;;                                    [value "label"]]]

      (let [attr (as-path attr)
            init-val (get-in obj attr)
            options (get obj options-key)

            _ (when-not (vector? options)
                (throw (ex-info "select options should be a vector" {:options options})))

            make-options (fn [options]
                           ;; FIXME: this should probably do some recursive action
                           ;; one may nest deeper than 1 lvl?
                           (for [[group-label group-options] options]
                             (if (string? group-options)
                               ;; single option
                               [:option {:value (-encode type group-label)} group-options]
                               ;; optgroup
                               [:optgroup {:label group-label}
                                (for [[value label] group-options]
                                  [:option {:value (-encode type value)
                                            :selected (when (= init-val value) true)}
                                   (str label)])])))

            select (dom/build [:select select-attrs (make-options options)])]

        (so/bind-change obj options-key
                        (fn [_ new]
                          (let [curval (get-in obj attr)]
                            (so/log "options changed" curval new)
                            (dom/reset select)
                            (doseq [option (make-options new)]
                              (dom/append select option))
                            (dom/set-value select (-encode type curval)))
                          ))

        (dom/on select :change
                (fn [ev]
                  (let [sv (dom/get-value select)
                        {:keys [valid value error]} (-validate type sv)]
                    (so/notify! obj :input-validated attr valid value error)
                    (if valid
                      (so/notify! obj :input-change attr value select)
                      (so/log "dom-select-grouped with invalid value?" obj attr value select)
                      ))))

        select
        )))

(defn dom-select
  ([obj attr type options-key]
     (dom-select obj attr type options-key {}))
  ([obj attr type options-key select-attrs]
     (when-not (satisfies? IInputType type)
       (throw (ex-info "dom select type must support protocol InputType" {:type type})))

     ;; options-key should point to options in obj (I may want to bind to them!)
     ;; options should be [[value "label"] [value "label"]

     (let [attr (as-path attr)
           init-val (get-in obj attr)
           multiple (:multiple select-attrs)

           options (get obj options-key)

           _ (when-not (vector? options)
               (throw (ex-info "select options should be a vector" {:options options})))

           make-options (fn [options]
                          (for [[value label] options]
                            [:option {:value (-encode type value)} (str label)]))

           select (dom/build [:select select-attrs (make-options options)])]

       (when-let [init-idx (index-of options #(= init-val (first %)))]
         (set! (.-selectedIndex select) init-idx))

       (so/bind-change obj options-key
                       (fn [_ new]
                         (let [curval (get-in obj attr)]
                           (so/log "options changed" curval new)
                           (dom/reset select)
                           (doseq [option (make-options new)]
                             (dom/append select option))
                           (dom/set-value select (-encode type curval)))
                         ))

       (dom/on select :change
               (fn [ev]
                 (let [sval (dom/get-value select)
                       {:keys [valid value error] :as result} (-validate type sval)]
                   (if valid
                     (so/notifty! obj :input-change attr value select)
                     (so/log "dom-select with invalid value?" obj attr sval value select result)
                     ))
                 ))

       select
       )))

(defn dom-input [obj attr type attrs]
  (when-not (satisfies? IInputType type)
    (throw (ex-info "dom input type must support protocol InputType" {:type type})))

  (let [attr (as-path attr)
        capture (:capture attrs #{:change})
        init-val (get-in obj attr)
        init-sval (:value attrs)
        init-sval (if (nil? init-val)
                    ""
                    (-encode type init-val))

        input (dom/build [:input (-> attrs
                                     (dissoc :capture :bind :type)
                                     (assoc :value init-sval
                                            :type (:type attrs "text")))])

        process-input (fn [ev-type e]
                        (let [sval (dom/get-value input)
                              {:keys [valid error value]} (-validate type sval)]
                          (so/notify! obj :input-validated attr valid (if valid value sval) error)
                          (when valid
                            (so/notify! obj ev-type attr value input)))
                        )]

    (when (:bind attrs)
      (so/bind-change obj attr
                       (fn [old new]
                         (dom/set-value input (-encode type new))
                         )))

    (when (contains? capture :enter)
      (dom/on input :keyup
              (fn [ev]
                (when (= 13 (.-keyCode ev))
                  (dom/ev-stop ev)
                  (process-input :input-enter ev)
                  ))))

    (when (contains? capture :change)
      (dom/on input :change #(process-input :input-change %)))

    input
    ))

(defn dom-checkbox [obj attr attrs]
  (let [attr (as-path attr)
        init-checked (get-in obj attr)
        input (dom/build [:input (assoc attrs :type "checkbox")])]

    (dom/check input init-checked)

    (when (:bind attrs)
      (so/bind-change obj attr
                       (fn [old new]
                         (dom/check input new)
                         )))

    (dom/on input :change (fn [e]
                            (so/notify! obj :input-change attr (dom/checked? input) input)))

    input
    ))

(defn dom-textarea [obj attr type attrs]
  (so/log obj attr type attrs)
  (when-not (satisfies? IInputType type)
    (throw (ex-info "dom input type must support protocol InputType" {:type type :attr attr :attrs attrs})))

  (let [attr (as-path attr)
        init-val (get-in obj attr)
        init-sval (:value attrs)
        init-sval (if (nil? init-val)
                    ""
                    (-encode type init-val))

        input (dom/build [:textarea (dissoc attrs :bind) init-sval])]

    (when (:bind attrs)
      (so/bind-change obj attr
                       (fn [old new]
                         (dom/set-value input (-encode type new))
                         )))

    (dom/on input :change (fn [e]
                            (let [sval (dom/get-value input)
                                  {:keys [valid error value]} (-validate type sval)]
                              (so/notify! obj :input-validated attr valid sval error)
                              (when valid
                                (so/notify! obj :input-change attr value input)))))

    input
    ))

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






