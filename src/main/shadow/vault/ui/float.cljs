(ns shadow.vault.ui.float
  (:refer-clojure :exclude (key))
  (:require-macros [cljs.core.async.macros :refer (alts!)])
  (:require [shadow.markup.react :as html]
            [shadow.react.component :as comp :refer (deffactory)]

            [shadow.vault.store :as store]
            [cljs.core.async :as async]

            [shadow.dom :as dom]
            [shadow.util :refer (go!)]
            [shadow.ui.position :as pos]
            [shadow.vault.dom :as vdom]))


(defn close [vault float-key]
  (update vault float-key assoc :open false))

(defn open [vault float-key anchor float-props]
  {:pre [(map? float-props)]}
  (update vault float-key assoc :open true :anchor anchor :float-props float-props))

(defn props->float-key [props]
  (::key props))

(deffactory host
  (-> {::store/read
       (fn [this vault {::keys [key] :as props}]
         (get vault key))

       ::comp/render
       (fn [{:keys [props data] :as this}]
         (let [key
               (::key props)

               {:keys [open anchor float-props]}
               data]

           (when open
             (html/div #js {:ref "float" :className "float-container"}
               (let [view-fn (::view props)]
                 (view-fn (assoc float-props ::key key)))))))

       ::comp/did-update
       (fn [{:keys [props data] :as this}]
         (let [key
               (::key props)

               float
               (comp/dom-node this "float")

               {:keys [anchor open]}
               data]

           (when open
             (let [{:keys [x y w h]} anchor]
               (pos/at-coordinate float [x (+ y h)]))))

         this)}
      (store/component)))

(deffactory remote*
  (-> {::comp/type
       ::remote

       ::comp/did-mount
       (fn [{::comp/keys [context] :keys [props] :as this}]
         (let [dom (dom/build [:div.float-root])]
           (dom/append dom)
           (vdom/mount dom (host props) context)
           (assoc this ::root dom)))

       ::comp/will-unmount
       (fn [{::keys [root] :as this}]
         (vdom/unmount root)
         (dom/remove root)
         (dissoc this ::root))

       ::comp/render
       (fn [this vault props]
         nil)}))

(defn remote [float-key float-view props]
  (remote* (assoc props
             ::key float-key
             ::view float-view)))
