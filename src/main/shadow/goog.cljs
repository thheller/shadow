(ns shadow.goog
  "workarrounds for working with goog stuff, mostly goog.ui.*"
  (:require [goog.events :as ge]
            [shadow.object :as so]))

(defn listen
  "listen to a goog event, if the obj is destroyed unlisten
   otherwise we leak event listeners and all the state they carry"
  [obj target ev callback]
  (let [key (ge/listen target ev callback)]
    (so/add-reaction! obj :destroy #(ge/unlistenByKey key))
    ))
