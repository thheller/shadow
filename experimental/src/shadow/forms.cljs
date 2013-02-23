(ns shadow.forms
  (:use-macros [shadow.macros :only (wait)])
  (:require [shadow.dom :as dom]
            [shadow.object :as obj]
            [shadow.ui :as ui]
            [shadow.xhr :as xhr]
            ;; [cljs.reader :as reader]
            ))

(defn read-string [s]
  {})

(defn display-errors-with [form handler]
  (swap! form assoc :default-error-display handler)
  form)

(defn- do-single-validation [value messages {:keys [fn message]}]
  (if (fn value)
    messages
    (conj messages message)))

(defn- trigger-remote-validation [form field-id value display-errors]
  (let [action (:xhr-action @form)
        req (xhr/get {:url action :params {:field (name field-id) :value (str value)}})]
    (wait [{:keys [valid errors]} req]
          (display-errors errors))
    ))

(defn do-validate [form field-id do-remote]
  (let [{:keys [validations
                control
                validate-remote
                optional]
         :as field} (get-in @form [:fields field-id])
         value (ui/get-value control)]
    (when-not (and optional (= value ""))
      (let [errors (distinct (reduce (partial do-single-validation value) [] validations))
            display-errors (:default-error-display @form)]
        (when-not (empty? errors)
          (swap! form assoc :valid false))

        (display-errors field-id (ui/get-dom control) errors)
        (when (and do-remote (empty? errors) validate-remote)
          (trigger-remote-validation
           form
           field-id
           value
           (fn [remote-errors]
             ;; dont bother displaying errors when the value has changed already
             (when (= value (ui/get-value control))
               (display-errors field-id (ui/get-dom control) remote-errors))))
          )))))

(defn validate-submit [form]
  (let [fields-to-validate (map :id (filter #(seq (:validations %)) (vals (get-in @form [:fields]))))]
    (obj/log "validate on submit" fields-to-validate)
    (swap! form assoc :valid true)
    (doseq [field-id fields-to-validate]
      (do-validate form field-id false))
    (:valid @form)))

(defn extract [form-id]
  (let [form-state (-> (str form-id "-form-state")
                       dom/by-id
                       dom/text
                       read-string)
        form-el (dom/by-id form-id)
        form-fields (reduce
                     (fn [form input]
                       (if-let [field-id (dom/attr input :data-field)]
                         (assoc form
                           (keyword field-id)
                           {:control input
                            :validations []
                            :validate-remote false
                            :optional false
                            :id (keyword field-id)
                            :type (or (dom/attr input :data-type) :string)})
                         form))
                     {}
                     (dom/form-elements form-el))


        form (atom {:data (:data form-state {})
                    :el form-el
                    :action (dom/attr form-el :action)
                    :xhr-action (dom/attr form-el :data-remote)
                    :errors (:errors form-state [])
                    :fields form-fields
                    :valid (:valid form-state true)})]

    (ev/listen form-el :submit (fn [e]
                                 (when (not (validate-submit form))
                                   (ev/stop e))
                                 ))
    form))

(defn optional [form field-id]
  (swap! form update-in [:fields field-id] assoc :optional true)
  form)


(defn- add-validation [form field-id validation]
  (let [field (get-in @form [:fields field-id])
        current-validations (:validations field)
        control (:control field)
        control-dom (ui/get-dom control)]
    (when (empty? current-validations)
      ;; only validate on blur when field is empty (change doesnt fire when u leave a field empty)
      ;; also never validate an empty field remotely, kinda pointless
      (ev/listen control-dom :blur (fn [e]
                                     (when (empty? (ui/get-value control))
                                       (do-validate form field-id false))))
      (ev/listen control-dom :change (fn [e]
                                       (do-validate form field-id true))))

    (swap! form update-in [:fields field-id :validations] conj validation)
    form
    ))

(defn validate [form field-id validation msg]
  (add-validation form field-id {:fn validation :message msg :remote false})
  form)

(defn validate-remote [form field-id]
  (swap! form update-in [:fields field-id] assoc :validate-remote true)
  form)

(defn display-errors-from-state! [form]
  (let [errors (reduce (fn [result {:keys [field message]}]
                         (let [current (get result field [])]
                           (assoc result field (conj current message)))
                         )
                       {}
                       (:errors @form []))
        fields (:fields @form {})
        display-errors (:default-error-display @form)]

    (doseq [[field-id messages] errors]
      (if-let [field (get-in fields field-id)]
        (let [control (:control field)]
          (display-errors field control messages))
        (obj/log "state has error in field which is not in dom" field-id)
        )))
  form)
