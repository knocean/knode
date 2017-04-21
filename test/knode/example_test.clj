(ns knode.example-test
  (:require [clojure.test :refer [deftest testing is]]
            [clojure.string :as string]
            [knode.state :refer [state]]
            [knode.core :as core]
            [knode.emit :as emit]
            [knode.cli :as cli]))

(def example-iri "https://example.com/ontology/EXAMPLE_0000002")

(deftest test-example-ontology
  (testing "Load example ontology"
    (knode.state/init-state!)
    (knode.state/testing-state!)
    (cli/load-state! "test/example/ontology/" "example")
    (is (= (:root-dir @state) "test/example/"))
    (is (= (->> (get-in @state [:terms example-iri :blocks])
                (map core/minimal-statement))
           [{:predicate {:iri "https://knotation.org/apply-template"}
             :object {:iri "http://example.com/template-1"}}
            {:predicate {:iri "https://example.com/ontology/EXAMPLE_name"}
             :object {:lexical "Two"}}
            {:template "http://example.com/template-1"
             :predicate {:iri "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"}
             :object {:iri "http://www.w3.org/2002/07/owl#Class"}}
            {:template "http://example.com/template-1"
             :predicate {:iri "http://www.w3.org/2000/01/rdf-schema#label"}
             :object {:lexical "Example Two"}}]))
    (is (= (emit/emit-ttl-terms (:env @state) (:context @state) (:terms @state))
           (string/trim (slurp "test/example/ontology/example.ttl"))))
    (is (= (emit/emit-kn-terms (:env @state) nil (:terms @state))
           (string/trim (slurp "test/example/ontology/example.kn"))))))
