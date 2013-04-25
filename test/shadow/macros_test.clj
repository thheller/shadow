(ns shadow.macros-test
  (:use clojure.test)
  (:use clojure.pprint)
  (:require [shadow.macros :as m :refer (domfn)]))


(pprint (macroexpand-1
         '(domfn [this]
            (let [x 1]
              [:div#id
               [:h1 "hello" "world"]
               [:ul.items
                (for [i [1 2 3]]
                  [:li (str "wtf" i)])]]))))


(pprint (macroexpand-1
         '(domfn [this]
         [:div.calc-window
          [:div.calc-panel.line
           [:div.unit.size1of4
            [:h2 "Suche anpassen"]
            [:div.cgroup
             [:label.clabel "icon"]
             [:div.clist
              (ui/dom-select this [:settings :device-id] (ui/int-type) :device-options)
              (ui/dom-select this [:settings :device-variant-id] (ui/int-type) :device-variant-options)
              ]]
            [:div.cgroup
             [:label.clabel "Vertragsart"]
             [:div.clist
              (ui/dom-select this [:settings :contract-type] (ui/keyword-type) :contract-type-options)]]

            [:div.cgroup
             [:label.clabel "Laufzeit"]
             [:div.clist
              (ui/dom-select this [:settings :max-contract-period] (ui/int-type) :max-contract-period-options)]]]

           [:div.unit.size1of4
            [:h2 "Suche eingrenzen"]
            [:div.cgroup
             [:label "Bevorzugte Netze"]
             [:div.networks
              [:label (ui/dom-checkbox this [:settings :network-d1] {:bind true}) "d1"]
              [:label (ui/dom-checkbox this [:settings :network-d2] {:bind true}) "d2"]
              [:label (ui/dom-checkbox this [:settings :network-o2] {:bind true}) "o2"]
              [:label (ui/dom-checkbox this [:settings :network-eplus] {:bind true}) "eplus"]] 
             ]
            [:div.cgroup
             [:label (ui/dom-checkbox this [:settings :price-limit-active] {:bind true}) "Preisgrenze festlegen"]]
            [:div.cgroup.price-limit
             (slider this [:settings :price-limit] "max. Effektivkosten pro Monat" :max250)
             ]]

           [:div.unit.size1of4
            [:h2 "Telefonie"]
            [:div.cgroup
             [:label "Ich telefoniere "]
             (ui/dom-select this [:settings :voice-preset] (ui/keyword-type) :voice-preset-options)]

            [:div.cgroup
             [:label.clabel "Festnetz"]
             [:div.clist
              (slider this [:settings :voice-festnetz] "Minuten pro Monat" :max500)]]
            [:div.cgroup
             [:label.clabel "Mobilfunk"]
             [:div.clist
              (slider this [:settings :voice-mobile] "Minuten pro Monat" :max500)]]
            [:div.cgroup
             [:label.clabel "SMS"]
             [:div.clist
              (slider this [:settings :sms] "SMS pro Monat" :max500)]]
            ]
           [:div.unit.size1of4.last-unit
            [:h2 "Mobiles Internet"]
            [:div.cgroup
             [:label "Ich surfe "]
             (ui/dom-select this [:settings :data-preset] (ui/keyword-type) :data-preset-options)]
            [:div.cgroup
             [:label.clabel "Inklusiv- Volumen"]
             [:div.clist
              (slider this [:settings :data-mb] "MB pro Monat" :max350)]]
            [:div.cgroup
             [:label "Geschwindigkeit "]
             (ui/dom-select this [:settings :data-speed] (ui/int-type) :data-speed-options)
             ]]]
          [:div.calc-results (pr-str (:settings this))]])))



