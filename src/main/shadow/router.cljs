(ns shadow.router
  (:require [shadow.vault.store :as store]
            [shadow.vault.dom :as vdom]
            [shadow.react.component :as comp]
            [clojure.set :as set]
            [clojure.string :as str])
  (:import [goog.history Html5History]))

(defprotocol TokenMatcher
  (token-match? [token test]))

(extend-protocol TokenMatcher
  string
  (token-match? [s test]
    (= s test))

  js/RegExp
  (token-match? [s test]
    (re-find s test)))

(defonce ^:private state-ref (volatile! nil))

(defn tokens-match? [match tokens]
  (let [max (count match)]
    (loop [i 0]
      (if (= i max)
        true
        (let [m (nth match i)
              t (nth tokens i)]
          (if-not (token-match? m t)
            false
            (recur (inc i)))
          )))))

(defn attempt-route [routes path]
  {:pre [(vector? routes)
         (str/starts-with? path "/")]}

  (if (= path "/")
    []
    (let [tokens
          (-> path
              (subs 1)
              (str/split #"/")
              (into []))

          route-state
          {:routes routes
           :tokens tokens
           :path []
           :consumed []}]

      (loop [{:keys [routes tokens path consumed] :as state}
             route-state]

        (let [tokens-left (count tokens)]
          ;; done routing
          (if (zero? tokens-left)
            path

            ;; check that routes are left for remaining tokens
            (do (when (nil? routes)
                  (throw (ex-info "no more routes" {:path path :tokens tokens :consumed consumed})))

                (-> (loop [[route & more-routes] routes
                           idx 0]
                      (if (nil? route)
                        (throw (ex-info "no route matched" {:consumed consumed :tokens tokens :idx idx}))

                        (let [{:keys [match children id]} route
                              tokens-req (count match)]

                          ;; not enough tokens left to match route, next
                          (if (> tokens-req tokens-left)
                            (recur more-routes (inc idx))

                            (let [tokens (subvec tokens 0 tokens-req)]

                              (if-not (tokens-match? match tokens)
                                ;; not all tokens matched, next
                                (recur more-routes (inc idx))

                                ;; all tokens matched, consume stuff and continue
                                (-> state
                                    (assoc :routes children)
                                    (update :tokens #(into [] (drop tokens-req %)))
                                    (update :consumed into tokens)
                                    (update :path conj [idx tokens]))
                                ))))))

                    ;; recur outer loop with updated state from ^
                    (recur)))))))))

(defn transition-plan
  [current next]
  ;; path is a structure of [[route-index route-tokens] ...]
  ;; test how many levels are identical and do not need to be re-routed
  (when (not= current next)
    (let [start-index
          (->> (map = current next)
               (take-while true?)
               (count))

          routes-to-exit
          (subvec current start-index)

          routes-to-enter
          (subvec next start-index)

          routes-to-keep
          (subvec current 0 start-index)]

      {:exit routes-to-exit
       :enter routes-to-enter
       :keep routes-to-keep}
      )))

(def path-xf
  (comp (map first)
        (interpose :children)))

(defn validate-state! [state route-id status]
  (when-not (map? state)
    (throw (ex-info (str "route " route-id "/" status " did not return the correct state") {})))
  state)

(defn router-state? [x]
  (true? (::router-state x)))

(defn install-handler! [match handler]
  #_ (fn route-handler [vault action]
    (dispatch-route-handler route vault action route-props)))

(defn remove-handler! [match])

(defn perform-exit
  [{:keys [active-routes] :as router-state}
   {:keys [match handler exit] :as route}]
  {:pre [(router-state? router-state)]}
  (js/console.log "ROUTE-STOP" match route)
  (let [routes-after
        (into [] (butlast active-routes))

        route-level
        (last active-routes)]

    (when (ifn? exit)
      (exit (:props route-level)))

    (when (ifn? handler)
      (remove-handler! match))

    (assoc router-state :active-routes routes-after)))

(defn dispatch-route-handler [active-route vault action]
  (let [{:keys [props route]}
        active-route

        {:keys [handler]}
        route]

    (cond
      (nil? handler)
      vault

      ;; route may have multiple handlers, execute in order
      ;; vector is ifn? so test for vector first
      (vector? handler)
      (reduce
        (fn [vault handler]
          (handler vault action props))
        vault
        handler)

      (ifn? handler)
      (handler vault action props)

      :else
      (throw (ex-info "invalid handler on route" {:route route}))
      )))

(defn perform-enter
  [{:keys [vault] :as router-state}
   {:keys [match handler enter props] :as route}
   raw-tokens]
  {:pre [(router-state? router-state)]}
  (js/console.log "ROUTE-START" match route)

  (let [route-props
        (if (ifn? props)
          (props raw-tokens)
          {})

        route-level
        {:tokens raw-tokens
         :id match
         :props route-props
         :route route}]

    (when (some? handler)
      (install-handler! match handler))

    (when (ifn? enter)
      (enter vault route-props))

    (-> router-state
        (update :active-routes conj route-level))))

(defn transition
  [{:keys [current routes] :as router-state} next]
  {:pre [(router-state? router-state)
         (vector? routes)
         (vector? current)
         (vector? next)]}
  (let [{:keys [keep enter exit] :as plan}
        (transition-plan current next)

        ;; move the root if current and next contain identical starts
        root
        (if (seq keep)
          (let [path
                (-> (into [] path-xf keep)
                    (conj :children))]
            (get-in routes path))
          routes)

        ;; stop from right -> left
        state-after-stop
        (loop [state router-state
               todo exit]
          (if-not (seq todo)
            state
            (let [path (into [] path-xf todo)
                  route (get-in root path)]
              (recur
                (perform-exit state route)
                (butlast todo)))))

        ;; blur the top when putting stuff on top
        _ (when (seq enter)
            (let [{:keys [blur] :as blur-route}
                  (-> state-after-stop :active-routes last :route)]
              (when blur
                (blur))))

        ;; start left -> right
        state-after-start
        (loop [state state-after-stop
               root root
               todo enter]
          (if-not (seq todo)
            state
            (let [[idx token :as path] (first todo)
                  route (nth root idx)]
              (recur
                (perform-enter state route token)
                (:children route)
                (rest todo)))))]

    ;; focus the top
    (let [{:keys [focus] :as focus-route}
          (-> state-after-start :active-routes last :route)]
      (when focus
        (focus)))

    (assoc state-after-start
      :current next
      :transition-at (count keep))
    ))

(defn navigate-to-token! [next-token]
  (let [{:keys [routes]}
        @state-ref

        label
        (str "NAVIGATE " next-token)]
    (js/console.group label)
    (try
      (let [next (attempt-route routes next-token)]
        (vswap! state-ref transition next))
      (catch :default e
        (js/console.error "navigation failed" e)))
    (js/console.groupEnd label)))

(defn stop! []
  (when-let [{:keys [root-dom history] :as router-state} @state-ref]
    (when root-dom
      (vdom/unmount root-dom))

    (.dispose history)

    (let [final-state
          (transition router-state [])]
      ;; FIXME: do something with final-state?
      (vreset! state-ref nil)
      )))

(declare sort-routes)

(defn sort-children [{:keys [children] :as route}]
  (if (seq children)
    (update route :children sort-routes)
    route))

(def route-keys #{:match :handler :enter :exit :focus :blur :props :children})

(defn validate-route [{:keys [match handler enter exit focus blur] :as route}]
  (let [test-keys (into #{} (keys route))]
    (when-not (set/subset? test-keys route-keys)
      (js/console.warn
        "route with invalid keys"
        route
        (set/difference test-keys route-keys))))

  (when-not (vector? match)
    (throw (ex-info "invalid match in route" route)))

  (when (and handler
             (not (or (ifn? handler)
                      (and (vector? handler)
                           (every? ifn? handler)))))
    (js/console.warn "invalid route handler" route))

  (when (and enter
             (not (ifn? enter)))
    (js/console.warn "invalid route enter" route))

  (when (and exit
             (not (ifn? exit)))
    (js/console.warn "invalid route exit" route))

  (when (and focus
             (not (ifn? focus)))
    (js/console.warn "invalid route focus" route))

  (when (and blur
             (not (ifn? blur)))
    (js/console.warn "invalid route blur" route))

  route)

(defn sort-routes [routes]
  (->> routes
       (map validate-route)
       (map sort-children)
       (sort-by #(count (:match %)))
       (reverse)
       (into [])))

(defn router-handler [vault action]
  (reduce
    (fn [vault active-route]
      (dispatch-route-handler active-route vault action))
    vault
    (:active-routes @state-ref)))

(defn control!
  "takes over control over current page, renders <root-fn> into <where>
   whenever a NAVIGATE event happens according to <routes>"
  [where ;; actual dom-node
   root-el
   {:keys [routes
           context
           start-token
           use-fragment
           path-prefix]
    :or {use-fragment true
         context {}}
    :as config}]
  {:pre [(sequential? routes)
         (boolean? use-fragment)
         (some? where)]}

  (stop!)

  (let [config
        (update config :routes sort-routes)

        history
        (doto (Html5History.)
          (.setUseFragment use-fragment))

        {::store/keys [vault]}
        context


        ]

    (when (nil? vault)
      (throw (ex-info "router needs store setup" context)))

    (let [vault
          (store/branch vault [router-handler])

          context
          (assoc context ::store/vault vault)]
      (vreset! state-ref
        (merge
          config
          {::router-state true
           :current []
           :handlers []
           :vault vault
           :root-dom where
           :active-routes []
           :active-token nil
           :history history}))

      (when (seq path-prefix)
        (.setPathPrefix history path-prefix))

      ;; MUST DO THIS BEFORE ADDING EVENT LISTENER
      ;; otherwise history will fire 2 events
      ;; first with token that was available when page was loaded
      ;; second with <start-token> when set
      (let [first-token
            (.getToken history)]
        (cond
          (and (= "" first-token) (seq start-token))
          (do (.replaceToken history start-token)
              (navigate-to-token! start-token))

          (seq first-token)
          (navigate-to-token! first-token)
          ))

      (.listen history js/goog.history.EventType.NAVIGATE
        (fn [e]
          ;; FIXME: only process navigation events, not calls to setToken?
          (when (.-isNavigation e)
            (let [next-token
                  (let [t (.-token e)]
                    (if (= t "")
                      "/"
                      t))]
              (navigate-to-token! next-token)
              ))))

      (.setEnabled history true)

      (js/console.warn "router/control!" context)
      (vdom/mount where root-el context))

    :all-your-browser-are-belong-to-us!
    ))

#_(defn path-rel* [mount-level new-tokens]
    (let [{:keys [active-routes]}
          @state-ref

          parent-tokens
          (->> (take mount-level active-routes)
               (mapcat :tokens)
               (into []))]

      (->> (map str new-tokens)
           (concat parent-tokens)
           (into [])
           (str/join "/")
           (str "/"))))

(defn path-abs* [new-tokens]
  (->> new-tokens
       (map str)
       (str/join "/")
       (str "/")))


#_(defn link-rel
    "returns a path relative to the current mount-level, used in render of components"
    [component new-path]
    {:pre [(sv/component? component)]}
    ;; FIXME: when not using fragment prepend path-prefix?
    (str "#" (path-rel* (sv/context component ::mount-level) new-path)))

(defn link-abs
  "returns a path relative to the root, used in render of components"
  [component new-tokens]
  {:pre [(comp/react? component)]}
  ;; FIXME: when not using fragment prepend path-prefix?
  (str "#" (path-abs* new-tokens)))

(defn navigate! [new-tokens]
  (let [history (:history @state-ref)
        new-path (path-abs* new-tokens)]
    (.setToken history new-path)

    ;; navigate here and not via event handler?
    (navigate-to-token! new-path)
    ))