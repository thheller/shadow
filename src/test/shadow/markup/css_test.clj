(ns shadow.markup.css-test
  (:require [clojure.test :refer (deftest is)]
            [clojure.pprint :refer (pprint)]
            [shadow.markup.css :as css :refer (defstyled)]
            [shadow.markup.css.impl.gen :as gen]
            [clojure.string :as str]))


(deftest test-css-generation
  (let [styles
        {:border "1px solid red"
         "&:hover"
         {:border "1px solid green"}
         "@media (max-width: 600px)"
         {}
         "@media (min-width: 600px)"
         {:border-width 2
          "&:hover"
          {:border-width 4}}}

        ;; FIXME: can't really guarantee that media will be after hover
        ;; the above will be an array-map so this works here
        [test-test test-test-hover media :as css-rules]
        (gen/css-rules styles "test" "test")]

    (is (= 3 (count css-rules)))
    (is (every? string? css-rules))
    (is (str/starts-with? test-test "test.test {"))
    (is (str/starts-with? test-test-hover "test.test:hover {"))
    (is (str/starts-with? media "@media"))

    #_(doseq [rule css-rules]
        (println rule))
    ))

(deftest empty-styles-generate-no-rules
  (is (empty? (gen/css-rules {} "test" "test")))
  (is (empty? (gen/css-rules {"&:hover" {}} "test" "test")))
  (is (empty? (gen/css-rules {"@media thing" {}} "test" "test"))))


(deftest element-should-not-require-props
  (let [el
        (css/element* "test" "test" (fn [_] {}))

        expected
        "<test class=\"test\"></test>"

        s1
        (el)

        s2
        (el {})
        ]

    (is (= expected s1))
    (is (= expected s2))
    ))