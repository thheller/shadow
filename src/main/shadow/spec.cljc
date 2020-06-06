(ns shadow.spec
  #?@(:cljs
      [(:require-macros [shadow.spec :as m])
       (:require [cljs.spec.alpha :as s])]
      :clj
      [
       (:require [clojure.spec.alpha :as s])]))

;; using deftype will break spec, must support IObj
(defrecord MapSpec [entries defined-keys closed?]
  ;; FIXME: what is this? just copied form other spec impls
  s/Specize
  (specize* [s] s)
  (specize* [s _] s)

  s/Spec
  (conform* [_ m]
    (if-not (map? m)
      ::s/invalid
      (loop [result m
             [{:keys [key required? spec]} & more :as x] entries]
        (if (nil? x)
          (if-not closed?
            result
            (if-not (every? defined-keys (keys m))
              ::s/invalid
              result))
          (let [present? (contains? m key)]
            (cond
              (and required? (not present?))
              ::s/invalid

              (not present?)
              (recur m more)

              :else
              (let [v (get m key)
                    cv (s/conform spec v)]
                (if (s/invalid? cv)
                  ::s/invalid
                  (recur (assoc m key cv) more)
                  ))))))))

  (explain* [_ path via in m]
    (if-not (map? m)
      [{:path path :pred 'map? :val m :via via :in in}]

      (let [undefined-keys
            (when closed?
              (->> (keys m)
                   (remove defined-keys)
                   (map (fn [key]
                          {:path path :pred `(not (contains? % ~key)) :val m :via via :in in}))
                   ))]

        (->> entries
             (mapcat (fn [{:keys [key required? spec-kw spec] :as x}]
                       (let [present? (contains? m key)]
                         (cond
                           (and required? (not present?))
                           [{:path path :pred `(contains? % ~key) :val m :via via :in in}]

                           (not present?)
                           []

                           :else
                           (let [v (get m key)]
                             (when-not (s/valid? spec v)
                               ;; if the defined spec was a keyword add to via
                               ;; just adds a :spec <the-kw> to the explain, nothing more
                               (let [via (if spec-kw (conj via spec-kw) via)]
                                 (s/explain* spec (conj path key) via (conj in key) v)
                                 )))))))
             (concat undefined-keys)
             (into [])))))

  ;; FIXME: no idea about the details of these
  ;; not required for what I'm doing at the moment
  (unform* [_ m]
    (throw (ex-info "TBD" {})))
  (gen* [_ overrides path rmap]
    (throw (ex-info "TBD" {})))
  (with-gen* [_ gfn]
    (throw (ex-info "TBD" {})))
  (describe* [_]
    (throw (ex-info "TBD" {}))))

#?(:clj
   (do (s/fdef shadow.spec/map-spec
         :args (s/+ (s/cat :key #{:req :opt :closed?}
                           :value #(or (map? %) (boolean? %)))))

       (defmacro map-spec [& {:keys [req opt closed?]}]
         (when (and req (not (map? req)))
           (throw (ex-info ":req must be a map" {:req req})))
         (when (and opt (not (map? opt)))
           (throw (ex-info ":opt must be a map" {:opt opt})))

         ;; FIXME: alias issue, might be shadow-build?
         (let [make-spec
               (if (:ns &env)
                 'cljs.spec.alpha/spec
                 'clojure.spec.alpha/spec)

               as-map-spec-entry
               (fn [[key spec] required?]
                 (-> {:key key
                      :required? required?
                      :spec `(~make-spec ~spec)}
                     (cond->
                       (keyword? spec)
                       (assoc :spec-kw spec))))

               req-def
               (map #(as-map-spec-entry % true) req)

               opt-def
               (map #(as-map-spec-entry % false) opt)

               map-entries
               (into [] (concat req-def opt-def))

               defined-keys
               (into #{} (map :key) map-entries)]

           (when (not= (count map-entries)
                       (count defined-keys))
             (throw (ex-info "invalid map-spec, key can't be in :req and :opt" {:keys defined-keys})))

           `(shadow.spec/->MapSpec ~map-entries ~defined-keys ~closed?)))))

(comment
  (s/def ::y (map-spec :req {:foo string? "bar" string?} :closed? true))

  (s/explain ::y {:foo "1" :x 1}))

(comment
  (deftype MySpec []
    s/Spec
    (conform* [_ x]
      ::s/invalid))

  (s/def ::x (MySpec.))

  (s/explain ::x :foo))

