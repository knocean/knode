(ns knode.natural-sort-test
  (:require [clojure.test :refer [deftest testing is]]
            [knode.util :as util]))

(deftest natural-compare-test
  (let [seq ["a" "b" "c" "1foo" "11foo" "111foo" "2bar" "22bar" "2222222bar" "11111baz" "3baz" "Baz"]]
    (is (= ["1foo" "2bar" "3baz" "11foo" "22bar" "111foo" "11111baz" "2222222bar" "a" "b" "Baz" "c"]
           (sort-by identity util/natural-compare seq)))))
