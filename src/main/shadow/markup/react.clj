(ns shadow.markup.react
  (:refer-clojure :exclude [for map meta time])
  (:require [cljs.analyzer :as ana])
  (:import (cljs.tagged_literals JSValue)))

;; will eventually become the main macro, for now just alias
(defmacro defstyled [& args]
  `(shadow.markup.css/defstyled ~@args))

(def dom-elements
  '[a
    abbr
    address
    area
    article
    aside
    audio
    b
    base
    bdi
    bdo
    big
    blockquote
    body
    br
    button
    canvas
    caption
    cite
    code
    col
    colgroup
    data
    datalist
    dd
    del
    dfn
    div
    dl
    dt
    em
    embed
    fieldset
    figcaption
    figure
    footer
    form
    input
    textarea
    option
    h1
    h2
    h3
    h4
    h5
    h6
    head
    header
    hr
    html
    i
    iframe
    img
    ins
    kbd
    keygen
    label
    legend
    li
    link
    main
    map
    mark
    marquee
    menu
    menuitem
    meta
    meter
    nav
    noscript
    object
    ol
    optgroup
    output
    p
    param
    pre
    progress
    q
    rp
    rt
    ruby
    s
    samp
    script
    section
    select
    small
    source
    span
    strong
    style
    sub
    summary
    sup
    table
    tbody
    td
    tfoot
    th
    thead
    time
    title
    tr
    track
    u
    ul
    var
    video
    wbr

    ;; svg
    circle
    ellipse
    g
    line
    path
    polyline
    rect
    svg
    text
    defs
    linearGradient
    polygon
    radialGradient
    stop
    tspan])

(defmacro for
  "same as clojure.core/for just not lazy"
  [bindings body]
  `(let [arr# (cljs.core/array)]
     (cljs.core/doseq ~bindings
       (.push arr# ~body))
     arr#
     ))

(defn maybe-nested-jsvalue [{:keys [style] :as x}]
  (-> x
      (cond->
        (and style (map? style))
        (assoc :style (JSValue. style)))
      (JSValue.)))

(defn dom-fn? [env [x & more :as form]]
  (and (symbol? x)
       (let [var (ana/resolve-var env x)]
         (and var (get-in var [:meta :shadow/dom-fn])))))

(defn gen-dom-macro [name]
  `(defmacro ~name [& args#]
     (let [env# ~'&env
           tag# ~(str name)
           [head# & tail#] args#]

       (cond
         (instance? JSValue head#)
         `(shadow.markup.react.impl.interop/create-element* ~(JSValue. (into [tag# head#] tail#)))

         (map? head#)
         `(shadow.markup.react.impl.interop/create-element* ~(JSValue. (into [tag# (maybe-nested-jsvalue head#)] tail#)))

         (= 'nil head#)
         `(shadow.markup.react.impl.interop/create-element* ~(JSValue. (into [tag# nil] tail#)))

         (string? head#)
         `(shadow.markup.react.impl.interop/create-element* ~(JSValue. (into [tag# nil head#] tail#)))

         ;; this is over-optimizing but it will be a very frequent occurence we can optimize
         (and (list? head#)
              (dom-fn? env# head#))
         `(shadow.markup.react.impl.interop/create-element* ~(JSValue. (into [tag# nil head#] tail#)))

         :else
         `(shadow.markup.react.impl.interop/create-element ~tag# ~(JSValue. args#))
         ))))

(defmacro define-element-macro []
  `(do ~@(clojure.core/map gen-dom-macro dom-elements)))

(define-element-macro)

(defn ^:private gen-dom-fn [tag]
  `(defn ~(with-meta tag {:shadow/dom-fn true})
     [& args#]
     (shadow.markup.react.impl.interop/create-element ~(name tag) args#)))

(defmacro define-elements []
  `(do
     ~@(clojure.core/map gen-dom-fn dom-elements)))

