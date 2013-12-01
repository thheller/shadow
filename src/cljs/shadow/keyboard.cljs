(ns shadow.keyboard
  (:require [shadow.dom :as dom]
            [shadow.object :as so]))


(def mod-keys #{16 ;;shift
                17 ;; ctrl
                18 ;; alt
                91 ;; meta
                })

(def char-names {8 "backspace"
                 9 "tab"
                 13 "enter"
                 27 "escape"
                 32 "space"
                 33 "pageup"
                 34 "pagedown"
                 35 "end"
                 36 "home"
                 37 "left"
                 38 "up"
                 39 "right"
                 40 "down"
                 46 "delete"
                 186 ";"
                 187 "="
                 188 ","
                 189 "-"
                 190 "."
                 191 "/"
                 192 "`"
                 219 "["
                 220 \\
                 221 "]"
                 222 "'"
                 112 "f1"
                 113 "f2"
                 114 "f3"
                 115 "f4"
                 116 "f5"
                 117 "f6"
                 118 "f7"
                 119 "f8"
                 120 "f9"
                 121 "f10"
                 122 "f11"
                 123 "f12"
                 })

(def FLAG-SHIFT 1)
(def FLAG-CTRL 2)
(def FLAG-ALT 4)
(def FLAG-META 8)

(defrecord KeyId [key flags])

(defn pretty-key-name [k]
  (or (char-names k) (.toLowerCase (String/fromCharCode k))))

(defn key-id-from-event [e]
  (let [flags 0
        flags (if (.-shiftKey e) (bit-or flags FLAG-SHIFT) flags)
        flags (if (.-ctrlKey e) (bit-or flags FLAG-CTRL) flags)
        flags (if (.-altKey e) (bit-or flags FLAG-ALT) flags)
        flags (if (.-metaKey e) (bit-or flags FLAG-META) flags)]

    (KeyId. (pretty-key-name (.-keyCode e))  flags)))

(def key-handlers (atom (list)))

(def ignore-key-events #{"INPUT" "SELECT" "BUTTON" "A"})

(defn handle-key-up [e]
  (let [key (.-keyCode e)]
    (when-not (or (contains? mod-keys key) (contains? ignore-key-events (.-nodeName (.-target e))))
      (so/log "keyboard/handle-key-up" e)
      (let [key-id (key-id-from-event e)]
        (loop [handlers @key-handlers]
          (if (empty? handlers)
            nil ;; stop
            (let [{:keys [oref keymap]} (first handlers)
                  handler (get keymap key-id)]
              (if handler
                (do
                  (so/log "keyboard handler: " key-id e oref handler)
                  (handler oref e))
                (recur (rest handlers)))
              )))))))

(defn parse-key-id [s]
  (let [parts (.split (.toLowerCase s) #"-|\+|\s")
        mods (set (butlast parts))
        key (last parts)
        flags 0
        flags (if (contains? mods "shift") (bit-or flags FLAG-SHIFT) flags)
        flags (if (contains? mods "ctrl") (bit-or flags FLAG-CTRL) flags)
        flags (if (contains? mods "alt") (bit-or flags FLAG-ALT) flags)
        flags (if (contains? mods "meta") (bit-or flags FLAG-META) flags)]
    (KeyId. key flags)
    ))

(defn remove-focus [oref]
  (swap! key-handlers (fn [x] (remove #(= oref (:oref %)) x))))

(defn push-focus [oref]
  (let [handlers (so/get-type-attr oref :keyboard)]
    (when (seq handlers)
      (let [keymap (reduce (fn [result [key-string handler-fn]]
                             (assoc result (parse-key-id key-string) handler-fn))
                           {}
                           (partition 2 handlers))]

        (swap! key-handlers conj {:oref oref :keymap keymap})
        (so/add-reaction! oref :destroy #(remove-focus %))
        ))

    ))

(defn hook! []
  ;; FIXME: this is broken in IE and otherwise inefficient
  ;;(dom/on (.-body js/document) "keyup" handle-key-up) true
  )

(hook!)
