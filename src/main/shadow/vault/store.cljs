(ns shadow.vault.store
  (:refer-clojure :exclude (empty keys))
  (:require-macros [shadow.vault.store :as m])
  (:require [clojure.set :as set]
            [cljs.spec :as s]
            [shadow.vault.env :as env]
            [shadow.vault.context :as ctx]
            [shadow.vault.schedule :as schedule]
            [shadow.react.component :as comp :refer (deffactory)]
            [goog.async.nextTick]))

;; KEY STUFF

(defprotocol IKey
  (key-init [key])
  (key-check-value! [key value]))

(defrecord VaultKey [tag id init-fn id-spec value-spec]
  IFn
  (-invoke [_ new-id]
    (when id-spec
      (when-not (s/valid? id-spec new-id)
        (js/console.warn "INVALID ID FOR KEY"
          key
          new-id
          (s/explain-str id-spec new-id)
          (s/explain-data id-spec new-id))

        (throw (ex-info "unacceptable id for key" {:key key :id new-id}))
        ))

    (VaultKey. tag new-id init-fn id-spec value-spec))

  IKey
  (key-init [key]
    (when init-fn
      (let [value
            (if (= id ::static)
              (init-fn)
              (init-fn id))]
        (key-check-value! key value)
        value
        )))

  (key-check-value! [_ value]
    (when value-spec
      (when-not (s/valid? value-spec value)
        (js/console.warn "INVALID VALUE FOR KEY"
          key
          (s/explain-str value-spec value)
          (s/explain-data value-spec value))

        (throw (ex-info "unacceptable value for key"
                 {:key key
                  :value value}))))))

(defn vault-key
  [value init-fn id-spec value-spec]
  {:pre [(or (nil? init-fn) (ifn? init-fn))]}
  (->VaultKey value ::static init-fn id-spec value-spec))

(defn key?
  ([x]
   (instance? VaultKey x))
  ([x y]
   (and (key? x)
        (key? y)
        (= :tag x) (:tag y))))

;; ACTIONS

(defrecord Action [id data])

(extend-type Action
  IPrintWithWriter
  (-pr-writer [this w opts]
    (-write w "#shadow.vault.store/action ")
    (-write w (pr-str [(:id this) (:data this)]))))

(defn read-action [[id data]]
  (Action. id data))

;; dont use directly, use via defaction
(defrecord ActionFactory [id schema]
  IFn
  (-invoke [_]
    (Action. id nil))

  (-invoke [_ data]
    (when ^boolean env/DEBUG
      (when-not (s/valid? schema data)
        (js/console.warn "INVALID ACTION" (s/explain-str schema data) (s/explain-data schema data))
        (throw (ex-info (str "INVALID ACTION\n" (s/explain-str schema data))
                 {:action id
                  :data data}))))

    (Action. id data)))

(defn action-factory [id spec]
  (when (and (some? spec)
             (not (or (s/spec? spec)
                      (qualified-keyword? spec)
                      )))
    (throw (ex-info (str "invalid spec for action " id) {:id id})))

  (ActionFactory. id spec))

;; VAULT STUFF

(defprotocol IStore
  (store-get [store])
  (store-swap! [store before after keys-new keys-updated keys-removed keys-touched]))

(defprotocol IBranch
  (branch [vault new-handlers]))

(defprotocol ITransact
  (transact! [vault actions]))

(defprotocol IKeys
  (keys [vault]))

(defprotocol ITransaction
  (tx-version [this])
  (tx-keys-removed [this])
  (tx-keys-new [this])
  (tx-keys-updated [this])
  (tx-commit-actions [this])

  (tx-process! [this action]))

(defprotocol ICommitHook
  (add-commit-hook! [this callback]))

(declare TransactedData)

(defn- tx-process-action [vault action handlers]
  (reduce
    (fn [vault handler-fn]
      (let [next (handler-fn vault action)]
        (when-not (instance? TransactedData next)
          (throw (ex-info (str "handler executing " action " did not return the vault!") {:handler-fn handler-fn :action action :result next})))

        next))
    vault
    (reverse handlers)))

(when ^boolean env/DEBUG
  (let [actual-fn tx-process-action]
    (set! tx-process-action
      (fn [vault action handlers]
        (let [group-label (str "TX-ACTION " (:id action))]
          (js/console.group group-label)
          (try
            (actual-fn vault action handlers)
            (finally
              (js/console.groupEnd group-label))
            ))))))

(deftype TransactedData
  ;; mutable stuff is because lazy init on lookup
  ;; which can't return a new vault since it needs
  ;; to return value
  [vault
   ^:mutable data
   handlers
   ^:mutable new-keys
   updated-keys
   removed-keys
   commit-actions

   ^:mutable completed?]
  IDeref
  (-deref [_]
    data)

  IKeys
  (keys [_]
    (->> (cljs.core/keys data)
         (filter key?)))

  ITransaction
  (tx-version [_]
    (::version data))
  (tx-keys-new [_]
    (persistent! new-keys))
  (tx-keys-updated [_]
    (persistent! updated-keys))
  (tx-keys-removed [_]
    (persistent! removed-keys))
  (tx-commit-actions [_]
    (persistent! commit-actions))

  (tx-process! [this action]
    (tx-process-action this action handlers))

  ICommitHook
  (add-commit-hook! [_ callback]
    (when completed?
      (throw (ex-info "transaction concluded, you need a different vault" {})))

    (TransactedData. vault data handlers new-keys updated-keys removed-keys (conj! commit-actions callback) completed?))

  ILookup
  (-lookup [_ key]
    (when completed?
      (throw (ex-info "transaction concluded, you need a different vault" {})))

    ;; FIXME: might not be a good idea to check this on every lookup
    (when-not (key? key)
      (throw (ex-info (str "need a key to get some from the vault") {:key key})))

    (let [value (-lookup ^not-native data key ::not-found)]
      (if (not= value ::not-found)
        value
        (when-let [init-value (key-init key)]
          ;; yay mutable :(
          (set! data (assoc data key init-value))
          (set! new-keys (conj! new-keys key))
          init-value
          ))))

  (-lookup [_ key default]
    (when completed?
      (throw (ex-info "transaction concluded, you need a different vault" {})))

    (when-not (key? key)
      (throw (ex-info "need a key to get some from the vault" {:key key})))
    ;; don't auto init when passed a default
    (-lookup ^not-native data key default))

  ICounted
  (-count [_]
    (when completed?
      (throw (ex-info "transaction concluded, you need a different vault" {})))

    (-count data))

  IMap
  (-dissoc [_ key]
    (when completed?
      (throw (ex-info "transaction concluded, you need a different vault" {})))

    (TransactedData.
      vault
      (-dissoc ^not-native data key)
      handlers
      new-keys
      updated-keys
      (conj! removed-keys key)
      commit-actions
      completed?))

  IAssociative
  (-assoc [this key value]
    (when completed?
      (throw (ex-info "transaction concluded, you need a different vault" {})))

    (when-not (key? key)
      (throw (ex-info "can only use vault keys to put stuff into the vault" {:key key :value value})))

    ;; FIXME: should it really check each write if anything changed?
    (if (identical? (-lookup ^not-native data key) value)
      this
      (do (key-check-value! key value)
          (TransactedData.
            vault
            (-assoc ^not-native data key value)
            handlers
            new-keys
            (conj! updated-keys key)
            removed-keys
            commit-actions
            completed?
            )))))

(deftype DefaultStore [store-id data-ref]
  IStore
  (store-get [_]
    @data-ref)

  (store-swap! [_ before after keys-new keys-updated keys-removed keys-touched]
    (when-not (identical? before @data-ref)
      (throw (ex-info "someone touched the vault while in a transation!" {})))

    (vreset! data-ref after)))

(defn- run-action [vault action]
  {:pre [(instance? Action action)]}

  (let [result (tx-process! vault action)
        #_(try
            (catch :default e
              (throw (ex-info (str "failed to run transaction step: " action) {:action action} e))))]

    ;; already checked in process!
    #_(when-not (instance? TransactedData result)
        (throw (ex-info "invalid action result, did not return vault" {:action action :type (type result)})))

    result))

(defn- run-actions [vault actions]
  (reduce run-action vault actions))

(defn read!
  "just read, does not record which keys where used"
  [vault key]
  ;; {:pre [(implements? VaultKey key)]}
  (when-not (key? key)
    (throw (ex-info "invalid key" {:key key})))

  ;; FIXME: should this init the key or not?
  (get @vault key)

  #_(let [value
          ]
      (if (not= ::not-found value)
        value
        (when-let [init-value (key-init key)]
          (js/console.warn "did init but did not store it in vault!")
          ;; (vswap! data-ref assoc key init-value)
          init-value
          ))))

(defrecord TransactionResult
  [actions
   keys-new
   keys-updated
   keys-removed
   keys-touched
   data-before
   data-after])

(deftype Vault [data-store handlers]
  IBranch
  (branch [this new-handlers]
    (Vault. data-store (into handlers new-handlers)))

  IDeref
  (-deref [_]
    (store-get data-store))

  ITransact
  (transact! [this actions]
    (let [data-before
          (store-get data-store)

          transacted-data
          (TransactedData.
            this
            data-before
            handlers
            (transient #{}) ;; new
            (transient #{}) ;; updated
            (transient #{}) ;; removed
            (transient []) ;; actions
            false)

          result
          (run-actions transacted-data actions)

          keys-new
          (tx-keys-new result)

          keys-updated
          (tx-keys-updated result)

          keys-removed
          (tx-keys-removed result)

          keys-touched
          (set/union keys-new keys-updated keys-removed)

          data-after
          (-> @result
              (update ::version inc))

          side-effects
          (tx-commit-actions result)]

      (store-swap! data-store data-before data-after keys-new keys-updated keys-removed keys-touched)

      (schedule/add-dirty-keys! keys-touched)

      ;; safeguard to ensure nobody stores a reference to a vault somewhere
      ;; and tries to write to it outside a transact! as any of those changes would be lost
      (set! (.-completed? result) true)

      (if-not (seq side-effects)
        ::success
        (do (js/goog.async.nextTick
              (fn []
                (try
                  (doseq [side-effect side-effects]
                    (side-effect this))
                  (catch :default e
                    (js/console.error "ERROR WHILE PROCESSING COMMIT ACTION" e)))))
            ::pending)))))

(defn on-commit!
  ([vault callback]
   (add-commit-hook! vault callback))
  ([vault callback a1]
   (add-commit-hook! vault #(callback % a1)))
  ([vault callback a1 a2]
   (add-commit-hook! vault #(callback % a1 a2)))
  ([vault callback a1 a2 & more]
   (add-commit-hook! vault #(apply callback % a1 a2 more))))

;; COMPONENT
;; FIXME: should have used ::data and ::prev-data?

(deftype ObservedData [^:mutable keys-used data]
  ILookup
  (-lookup [_ key]
    (when (nil? key)
      (throw (ex-info "cannot read nil key" {})))
    (set! keys-used (conj keys-used key))
    (-lookup data key))
  (-lookup [_ key default]
    (when (nil? key)
      (throw (ex-info "cannot read nil key" {})))
    (set! keys-used (conj keys-used key))
    (-lookup data key default)))

(defn do-read [component props-for-read]
  (let [{::comp/keys [ref]
         ::keys [vault]
         :keys [prev-data]}
        component

        keys-used-before
        (::keys-used component)

        {::keys [version] :as data}
        @vault

        observed-data
        (ObservedData. #{} data)

        data
        (comp/query component ::read observed-data props-for-read)

        keys-used
        (.-keys-used observed-data)

        old-keys
        (set/difference keys-used-before keys-used)

        new-keys
        (set/difference keys-used keys-used-before)]

    (schedule/link-ref-to-keys! ref new-keys)

    (schedule/unlink-ref-from-keys! ref old-keys)

    ;;(js/console.log "read" (-> component ::comp/config ::comp/type) keys-used (not= prev-data data))

    (assoc component
      ::keys-used keys-used
      ::dirty-data? (not= prev-data data)
      ::data-version version
      :data data)))

(defn component-props-dirty?
  [{::comp/keys [config]
    :keys [pending-props prev-props]
    :as component}]
  (if-let [compare (:props-compare config)]
    (not (every? #(= (get pending-props %) (get prev-props %)) compare))
    (not= pending-props prev-props)))

(defn component-will-receive-props
  [{:keys [pending-props] :as component}]
  (if (comp/query component ::props-dirty?)
    (-> component
        (assoc ::dirty-props? true)
        (do-read pending-props))
    component))

(defn component-schedule-update [{:keys [props] :as this}]
  (let [{::keys [dirty-data?] :as new-this}
        (do-read this props)]
    (when dirty-data?
      (schedule/add-dirty-component! (::comp/ref this)))

    new-this))

(defn component-should-update? [{::keys [dirty-props? dirty-data?] :as this}]
  (when (or dirty-props? dirty-data?)
    ;; (js/console.log "should-update?" (-> this ::comp/config ::comp/type) dirty-props? dirty-data?)
    true
    ))

(defn component-constructor
  [this react]
  (let [{::comp/keys [ref config context]}
        this

        {::keys [vault]}
        context

        _ (when-not vault
            (throw (ex-info "store component without vault in context" {:component (::comp/type this)
                                                                        :react react})))

        handlers
        (->> (::handlers config)
             (map (fn [handler-fn]
                    (fn [vault action]
                      (handler-fn vault action (comp/get-shadow @ref)))))
             (into []))

        vault
        (if handlers
          (branch vault handlers)
          vault)

        context
        (assoc context
          ::ctx/depth (-> context ::ctx/depth inc)
          ::vault vault)]

    (assoc this
      ::comp/context context
      ::vault vault)))

(defn component-mark-clean [{:keys [data] :as this}]
  (schedule/did-render! (::comp/ref this))

  (assoc this
    :prev-data data
    ::dirty-props? false
    ::dirty-data? false))

(defn component-will-mount [{:keys [props] :as component}]
  (do-read component props))

(defn component-will-unmount [component]
  (schedule/unlink-ref-from-keys! (::comp/ref component) (::keys-used component))
  component)

;; react cycle new
;; ::comp/init
;; ::comp/will-mount
;; ::comp/render
;; ::comp/did-mount

;; react cycle update
;; ::comp/will-receive-props
;; ::comp/should-update?
;; ::comp/will-update
;; ::comp/render
;; ::comp/did-update

(defn set-render-fn [{::keys [render] :as config}]
  (assoc config
    ::comp/render
    (fn store-render [{::keys [vault] :keys [props data] :as this}]
      (render this vault props data)
      )))

(defn component
  [{::comp/keys
    [render
     will-receive-props
     should-update?]
    :as config}]

  {:pre [(nil? will-receive-props)
         (nil? should-update?)]}

  ;; FIXME: better validation, also can't have those other fns

  (-> config
      (assoc
        ::comp/will-receive-props
        component-will-receive-props

        ::schedule/update
        component-schedule-update

        ::props-dirty?
        component-props-dirty?

        ::comp/should-update?
        component-should-update?)

      (comp/after ::comp/did-mount component-mark-clean)
      (comp/after ::comp/did-update component-mark-clean)
      (comp/after ::comp/constructor component-constructor)

      (comp/after ::comp/will-mount component-will-mount)
      (comp/after ::comp/will-unmount component-will-unmount)

      (cond->
        (nil? render)
        (set-render-fn))
      ))

(deffactory root*
  {::comp/constructor
   (fn [{::comp/keys [context]
         :keys [props]
         :as this}
        react]
     (update this ::comp/context assoc ::vault (:vault props)))

   ::comp/render
   (fn [this]
     (get-in this [:props :root-el]))})

(defn root
  "prefer to inject context directly"
  [data-ref handlers root-el]
  (root* {:vault (Vault. data-ref handlers)
          :root-el root-el}))

(defn context [ctx data-ref handlers]
  (assoc ctx ::vault (Vault. data-ref handlers)))

(defn empty
  "creates a new empty store
   this should be treated totally opaque and you should never directly interact with it
   - only pass into context"
  ([]
   (empty ::default))
  ([store-id]
   (DefaultStore. store-id (volatile! {::version 0}))))

(defn vault
  ([handlers]
   (vault (empty) handlers))
  ([data-ref handlers]
   (Vault. data-ref handlers)))

