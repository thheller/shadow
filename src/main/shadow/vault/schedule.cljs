(ns shadow.vault.schedule
  (:require [shadow.react.component :as comp]
            [shadow.vault.env :as env]
            [goog.async.nextTick]
            [goog.dom.animationFrame.polyfill :as raf-polyfill]
            [clojure.set :as set]))

(when (exists? js/window)
  (raf-polyfill/install))

(defn set-conj [s x]
  (if (nil? s)
    #{x}
    (conj s x)))

(def render-seq-ref
  (volatile! 0))

;; what the frame! is going to render
(def frame-pending-ref
  (volatile! #{}))

;; what the frame! did render
(def frame-rendered-ref
  (volatile! #{}))

;; keeps track of which components need to be updated if key changes
;; FIXME: implement some kind of GC to remove keys from the world that
;; are no longer used, but leave to the key decide if it wants to die
;; right now we are leaking memory since we keep everything
(def key->components-ref
  (volatile! {}))

(def dirty-components-ref
  (volatile! #{}))

(def dirty-keys-ref
  (volatile! #{}))

;; render already queued?
(def render-pending-ref
  (volatile! false))

(defn- do-render-component! [ref]
  (when-not (contains? @frame-rendered-ref ref)
    (comp/force-update ref)
    ))

(defn- notify-components-of-dirty-keys! [ref]
  ;; this should cause a data read
  ;; if the data changed in any way, the component should call add-dirty-component!
  ;; which will then cause it to render later
  (comp/call-ref ref ::update))

;; FIXME: rewrite to requestIdleCallback and only do work while time is available
(defn- frame! []
  (vreset! frame-rendered-ref #{})

  (let [dirty-keys
        @dirty-keys-ref

        links
        @key->components-ref

        maybe-dirty-components
        (reduce
          (fn [queue key]
            (if-let [linked (get links key)]
              (do (when (> (count linked) 20)
                    #_ (js/console.warn "many components linked to key" key linked))
                  (set/union queue linked))
              queue))
          #{}
          dirty-keys)]

    (js/console.log "dirty-keys" (count dirty-keys) "maybe-dirty-components" (count maybe-dirty-components))

    (vreset! dirty-keys-ref #{})
    (run! notify-components-of-dirty-keys! maybe-dirty-components))

  (let [dirty-components
        @dirty-components-ref]

    (vreset! dirty-components-ref #{})

    (vreset! render-pending-ref false)

    (vreset! frame-pending-ref dirty-components)

    ;; (js/console.log "dirty-components" dirty-components)

    (run! do-render-component! dirty-components)))

(when ^boolean env/DEBUG
  (let [actual-frame frame!]
    (set! frame!
      (fn frame!-debug []
        (let [frame-id (vswap! render-seq-ref inc)
              label (str "RENDER #" frame-id " (" (count @dirty-components-ref) " queued)")]
          (js/console.group label)
          (js/console.time "render/time")
          (try
            (actual-frame)

            (js/console.log "rendered" (count @frame-rendered-ref) "components")
            (finally
              (js/console.timeEnd "render/time")
              (js/console.groupEnd)
              )))))))

(defn queue-frame! []
  (when-not @render-pending-ref
    (vreset! render-pending-ref true)
    (js/window.requestAnimationFrame frame!)))

(defn add-dirty-component! [ref]
  {:pre [(comp/ref? ref)]}
  (vswap! dirty-components-ref conj ref))

(defn add-dirty-keys! [keys]
  {:pre [(set? keys)]}
  (vswap! dirty-keys-ref set/union keys)
  (queue-frame!))

(defn did-render! [ref]
  {:pre [(comp/ref? ref)]}
  (vswap! frame-rendered-ref conj ref))

(defn link-ref-to-keys! [ref new-keys]
  (doseq [key new-keys]
    (vswap! key->components-ref update key set-conj ref)))

(defn unlink-ref-from-keys! [ref old-keys]
  (doseq [key old-keys]
    (vswap! key->components-ref update key disj ref)))

