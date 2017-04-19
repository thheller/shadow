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
  (-> (comp/get-component-config x)
      (::comp/type)))

(defn root? [x]
  (and x (keyword? (multi-id x))))

(defn reset [multi]
  {:pre [(root? multi)]}
  (let [multi-id (multi-id multi)]
    (assert (keyword? multi-id))
    (vswap! registry-ref update :selected dissoc multi-id)
    (queue-root-update! multi-id)
    ))

(defn select [multi state-id props]
  {:pre [(root? multi)
         (keyword? state-id)
         (map? props)]}
  (let [multi-id (multi-id multi)]
    (assert (keyword? multi-id))
    ;; (js/console.warn "select multi-id" multi-id state-id props)

    (vswap! registry-ref update :selected
      assoc multi-id {:state-id state-id
                      :state-props props})

    (queue-root-update! multi-id)
    ))


(defn extend
  [{::keys [states]
    ::comp/keys [render]
    :as config}]
  ;; Root defines a render, component cannot define a render itself
  ;; FIXME: better validation
  {:pre [(map? states)
         (nil? render)]}

  (-> config
      (comp/after ::comp/will-mount
        (fn [{::comp/keys [ref config] :as this}]
          (let [multi-id (::comp/type config)]
            (when-let [current (get-in @registry-ref [:mounted multi-id])]
              (throw (ex-info "tried to mount multi more than once" {:id multi-id :current current})))

            (vswap! registry-ref update :mounted assoc multi-id ref)

            this)))
      (comp/after ::comp/will-unmount
        (fn [this]
          (let [multi-id (get-in this [::comp/config ::comp/type])]
            (vswap! registry-ref update :mounted dissoc multi-id)
            this)))
      (assoc
        ::comp/render
        (fn [{::comp/keys [config] :keys [props] :as this}]
          (let [multi-id
                (::comp/type config)

                registry
                @registry-ref

                {:keys [state-id state-props]}
                (get-in registry [:selected multi-id])]

            (when state-id
              (let [props
                    (merge props state-props)

                    state-render
                    (get states state-id)]

                (if-not state-render
                  (do (js/console.warn "multi in invalid state" (::comp/type config) state-id (keys states))
                      (html/div "INVALID STATE MULTI"))
                  (state-render props)))))))))
