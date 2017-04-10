(ns knode.core-test
  (:require [clojure.test :refer [deftest testing is]]
            [knode.core :refer [parse-declaration]]))


(deftest test-parse-declaration
  (testing "parse-declaration returns the expected for basic declaration forms"
    (is (= {:type :prefix :name "rdf"
            :target {:type :iri :iriref "http://www.w3.org/1999/02/22-rdf-syntax-ns#"}}
           (parse-declaration "@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>")))
    (is (= {:type :base :target {:type :iri :iriref "http://example.com/"}}
           (parse-declaration "@base <http://example.com/>")))
    (is (= {:name "type"
            :type :label
            :datatype {:type :string, :string "link"}
            :target {:type :prefixed-name
                     :prefix "rdf" :name "type"
                     :datatype {:type :string, :string "link"}}}
           (parse-declaration "@label type: rdf:type > link")))
    (is (= {:type :graph :target {:type :iri :iriref "http://example.com/graph"}}
           (parse-declaration "@graph <http://example.com/graph>")))))

;; TODO
;; Things that I've fixed, that I should therefore write test-cases for
;; - test that expand-string uses the iriref of base, not its identity
