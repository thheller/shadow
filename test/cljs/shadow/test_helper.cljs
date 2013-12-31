(ns shadow.test-helper
  (:require-macros [shadow.macros :refer (log)])
  (:require [cemerick.cljs.test :as t :refer-macros (is deftest)]
            [shadow.object :as so]
            [shadow.dom :as dom]
            [clojure.string :as str]))

(defn wipe-objects []
  (let [root (dom/by-id "root")] 
    (doseq [node (dom/children root)
            :let [obj (so/get-from-dom node)]
            :when obj]
      (log "DESTROY LEFTOVER" node)
      (so/destroy! obj))
    (dom/reset root)))

;; test helper ui

(dom/append [:h1 "Tests"])

(let [log (dom/append [:div.log])]
  (t/set-print-fn! (fn [msg]
                     (let [s (str/trim msg)]
                       (when (seq s)
                         (dom/append log [:pre.log-line s])))))

  (so/define ::test-button
    :dom (fn [{:keys [name] :as this}]
           [:button {:type "button"} (str "test: " name)])

    :dom/events [:click (fn [{:keys [var] :as this}]
                          (dom/reset log)
                          (wipe-objects)
                          (t/test-var var))]))

(doseq [[ns tests] (->> @t/registered-tests
                        (sort-by first))]

  (dom/append [:h2 (str "NS: " ns)])
  (doseq [x (->> tests
                 (sort-by #(:name (meta %))))
          :let [{:keys [name]} (meta x)]]
    (dom/append (so/create ::test-button {:var x :name name}))
    ))




