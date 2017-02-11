(ns shadow.react.multi
  "EXPERIMENTAL - DO NOT USE"
  (:refer-clojure :exclude (extend))
  (:require [shadow.react.component :as comp]
            [shadow.markup.react :as html]
            [shadow.vault.schedule :as schedule]))

(defonce ^:private registry-ref
  (volatile!
    ;; map of {multi-id {:state-id state-id :state-props state-props}}
    {:selected {}

     ;; {multi-id component-ref}
     :mounted {}
     }))

(defn- queue-root-update! [multi-id]
  (when-let [mounted (get-in @registry-ref [:mounted multi-id])]
    (schedule/add-dirty-component! mounted)
    (schedule/queue-frame!)
    ))

(defn- multi-id [x]
  (-> (comp/get-factory-config x)
      (::comp/type)))

(defn root? [x]
  (and x (comp/is-shadow-factory? x)))

(defn reset [multi]
  {:pre [(root? multi)]}
  (let [multi-id (multi-id multi)]
    (vswap! registry-ref update :selected dissoc multi-id)
    (queue-root-update! multi-id)
    ))

(defn select [multi state-id props]
  {:pre [(root? multi)
         (keyword? state-id)
         (map? props)]}
  (let [multi-id (multi-id multi)]
    ;; (js/console.log "select multi-id" multi-id state-id props)

    (vswap! registry-ref update :selected
      assoc multi-id {:state-id state-id
                      :state-props props})

    (queue-root-update! multi-id)
    ))


(defn extend
  [{::keys [states]
    ::comp/keys [type render]
    :as config}]
  ;; Root defines a render, component cannot define a render itself
  ;; FIXME: better validation
  {:pre [(map? states)
         (nil? render)]}

  (let [multi-id type]

    (assoc config
      ::comp/will-mount
      (fn [{::comp/keys [ref] :as this}]
        (when-let [current (get-in @registry-ref [:mounted multi-id])]
          (throw (ex-info "tried to mount multi more than once" {:id multi-id :current current})))

        (vswap! registry-ref update :mounted assoc multi-id ref)

        this)

      ::comp/will-unmount
      (fn [this]
        (vswap! registry-ref update :mounted dissoc multi-id)
        this)

      ::comp/render
      (fn [{:keys [props] :as this}]
        (let [registry
              @registry-ref

              {:keys [state-id state-props]}
              (get-in registry [:selected multi-id])]

          (when state-id
            (let [props
                  (merge props state-props)

                  state-render
                  (get states state-id)]

              (when-not state-render
                (throw (ex-info "multi in invalid state" {:id state-id :states (keys states)})))

              (state-render props)))
          )))))
