(ns knode.example-test
  (:require [clojure.test :refer [deftest testing is]]
            [knode.state :refer [state]]
            [knode.core :as core]
            [knode.emit :as emit]
            [knode.cli :as cli]))

(def example-iri "https://example.com/ontology/EXAMPLE_0000001")

(deftest test-example-ontology
  (testing "Load example ontology"
    (knode.state/init-state!)
    (knode.state/testing-state!)
    (cli/load-state! "test/example/ontology/" "example")
    (is (= (:root-dir @state) "test/example/"))
    (is (= (emit/emit-ttl
            (:env @state)
            (:context @state)
            {:iri "https://example.com/ontology/EXAMPLE_0000001"}
            (get-in @state [:terms example-iri :blocks]))
           "@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
@prefix owl: <http://www.w3.org/2002/07/owl#> .
@prefix obo: <http://purl.obolibrary.org/obo/> .
@prefix EXAMPLE: <https://example.com/ontology/EXAMPLE_> .

EXAMPLE:0000001
  rdf:type owl:Class
; rdfs:label \"Example One\"
; obo:IAO_0000118 \"ex 1\"
; rdfs:subClassOf owl:Thing
."))))
