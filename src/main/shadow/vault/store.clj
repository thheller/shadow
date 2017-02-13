(ns shadow.vault.store
  (:require [clojure.spec :as s]))

(s/def ::key
  (s/cat
    :key-name
    simple-symbol?

    :doc
    (s/? string?)

    :body
    (s/*
      (s/cat
        :key
        #{:init :spec :id-spec :value-spec}

        :value
        any?))))

(defmacro defkey [& form]
  (let [{:keys [key-name body] :as form-data}
        (s/conform ::key form)

        key-id
        (keyword (str *ns*) (str key-name))

        key-data
        (reduce
          (fn [m {:keys [key value]}]
            (assoc m key value))
          {}
          body)]

    `(def ~key-name
       (shadow.vault.store/vault-key
         ~key-id
         ~(:init key-data)
         ~(:id-spec key-data)
         ~(or (:value-spec key-data)
              (:spec key-data))))
    ))

(s/fdef defkey
  :args ::key)

(defmacro defaction
  [name-sym & [spec]]
  (let [id (keyword (str *ns*) (str name-sym))]
    `(def ~name-sym
       (if ~(with-meta 'shadow.vault.env/DEBUG {:tag 'boolean})
         (shadow.vault.store/action-factory ~id ~spec)
         (shadow.vault.store/action-factory ~id nil))
       )))

(defmacro action-case [action-input & cases]
  ;; FIXME: should validate cases form
  (let [id (gensym "action_id_")
        action (gensym "action_data_")
        default (last cases)
        cases (->> cases (butlast) (partition 2))
        cond-pairs
        (->> cases
             (mapcat (fn [[binding body]]
                       (when-not (vector? binding)
                         (throw (ex-info "invalid action case, expected [action data-binding]" {:binding binding})))

                       (let [[action-sym data-binding] binding]
                         (when-not (symbol? action-sym)
                           (throw (ex-info "conditional in action-case must be symbol" {:action action-sym :body body})))
                         [`(= ~id (:id ~action-sym))
                          (if data-binding
                            `(let [~data-binding (:data ~action)]
                               ~body)
                            body)]))))]

    `(let [~action ~action-input
           ~id (:id ~action)]
       (cond
         ~@cond-pairs

         ::unhandled-action
         (do ~default)))))
