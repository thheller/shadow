(ns shadow.markup.react
  (:refer-clojure :exclude [for map meta time])
  (:import (cljs.tagged_literals JSValue)))

;; FIXME: can do this due to cyclic dependency
;; will probably move all react related stuff here
;; and hiccup related stuff somewhere else
;; and give up on the plan of having one namespace for both platforms
;; one namespace mean that every dependency for the clojure version
;; also becomes dependency of cljs version which sucks.
#_ (defmacro defstyled [& args] `(shadow.markup.css/defstyled ~@args))

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
  [bindings & body]
  `(doall
     (cljs.core/for ~bindings
       ~@body)))

(defn gen-dom-macro [name]
  `(defmacro ~name [& args#]
     (let [tag# ~(str name)
           [head# & tail#] args#]

       (cond
         (instance? JSValue head#)
         `(shadow.markup.react/create-element* ~tag# ~head# ~(JSValue. tail#))

         (map? head#)
         `(shadow.markup.react/create-element* ~tag# ~(JSValue. head#) ~(JSValue. tail#))

         (= 'nil head#)
         `(shadow.markup.react/create-element* ~tag# ~(JSValue. {}) ~(JSValue. tail#))

         :else
         `(shadow.markup.react/create-element ~tag# ~(JSValue. args#))
         ))))

(defmacro define-element-macro []
  `(do ~@(clojure.core/map gen-dom-macro dom-elements)))

(define-element-macro)

(defn ^:private gen-dom-fn [tag]
  `(defn ~tag
     [& args#]
     (shadow.markup.react/create-element ~(name tag) args#)))

(defmacro define-elements []
  `(do
     ~@(clojure.core/map gen-dom-fn dom-elements)))

