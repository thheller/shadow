(ns shadow.vault.ui.dialog
  (:refer-clojure :exclude (key))
  (:require-macros [cljs.core.async.macros])
  (:require [shadow.vault.store :as store :refer (defaction)]
            [shadow.vault.dom :as vdom]
            [shadow.markup.react :as html]
            [shadow.react.component :as comp :refer (deffactory)]
            [shadow.dom :as dom]
            [shadow.util :refer (go!)]
            [cljs.core.async :as async]
            ))

(defaction $close)

(defaction $backdrop-click)

(defn close [vault dialog-key]
  (update vault dialog-key assoc :open false))

(defn open [vault dialog-key dialog-props]
  (update vault dialog-key assoc :open true :dialog-props dialog-props))

(defn container [props & children]
  (html/div #js {:className "dialog-container"} children))

(defn header [props & children]
  (html/div #js {:className "dialog-header"} children))

(defn body [props & children]
  (html/div #js {:className "dialog-body"} children))

(defn footer [props & children]
  (html/div #js {:className "dialog-footer"} children))

(deffactory host
  ::comp/mixins
  [store/mixin]

  ::store/read
  (fn [this vault {::keys [key] :as props}]
    (get vault key))

  ::comp/render
  (fn [{::store/keys [vault]
        :keys [props data]
        :as this}]
    (let [key
          (::key props)

          {:keys [open dialog-props]}
          data]

      (when open
        (html/div nil
          (html/div #js {:className "dialog-backdrop"
                         :ref "backdrop"
                         :onClick
                         (fn [e]
                           (.preventDefault e)
                           (store/transact! vault [($backdrop-click)]))})

          (let [view-fn (::view props)]
            (view-fn (assoc dialog-props ::key key)))))))

  ::comp/did-update
  (fn [{:keys [data prev-data] :as this}]
    (let [was-open
          (:open prev-data)

          is-open
          (:open data)]

      (cond
        (and (not was-open) is-open)
        (dom/set-style js/document.body {:overflow "hidden"})

        (and was-open (not is-open))
        (dom/remove-style js/document.body :overflow)

        :else
        nil))

    this
    ))

(deffactory remote*
  ::comp/did-mount
  (fn [{::comp/keys [context] :keys [props] :as this}]
    (let [dom (dom/build [:div.dialog-root])]
      (dom/append dom)
      (vdom/mount dom (host props) context)

      (assoc this ::root dom)))

  ::comp/will-unmount
  (fn [{::keys [root] :as this}]
    (vdom/unmount root)
    (dom/remove root)
    (dissoc this ::root))

  ::comp/render
  (fn [this]
    nil))

(defn remote [dialog-key dialog-view props]
  (-> props
      (assoc
        ::key dialog-key
        ::view dialog-view)
      (remote*)))
