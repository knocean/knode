(ns knode.example-test
  (:require [clojure.test :refer [deftest testing is]]
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
    (is (= (emit/emit-ttl
            (:env @state)
            (:context @state)
            {:iri example-iri}
            (get-in @state [:terms example-iri :blocks]))
           "@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
@prefix owl: <http://www.w3.org/2002/07/owl#> .
@prefix obo: <http://purl.obolibrary.org/obo/> .
@prefix kn: <https://knotation.org/> .
@prefix EXAMPLE: <https://example.com/ontology/EXAMPLE_> .

EXAMPLE:0000002
  kn:apply-template <http://example.com/template-1>
; EXAMPLE:name \"Two\"
; rdf:type owl:Class
; rdfs:label \"Example Two\"
."))
    (is (= (emit/emit-kn
            (:env @state)
            nil
            {:iri example-iri}
            (->> (get-in @state [:terms example-iri :blocks])
                 (remove :template)))
           ": EXAMPLE:0000002
template: example class
name: Two"))))
