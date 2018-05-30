(ns com.knocean.knode.pages.resources-test
  (:require [clojure.test :refer [deftest testing is use-fixtures]]
            [clojure.java.io :as io]
            [clojure.java.jdbc :as jdbc]

            [me.raynes.fs :as fs]
            [net.cgrand.enlive-html :as html]

            [com.knocean.knode.state :refer [state] :as st]
            [com.knocean.knode.loader :as loader]
            [com.knocean.knode.pages.resources :as res]))

(def example-config
  {:project-dir "test/example/"
   :database-url "jdbc:sqlite:target/test.db"})

(def example-resource
  {:label "example"
   :title "Example"
   :description "Example description"
   :homepage "http://example.com"})

(defn delete-test-db
  []
  (try
    (fs/delete "target/test.db")
    (catch Exception e)))

(defn example-state-fixture
  [f]
  (let [saved-state @state]
    (delete-test-db)
    (->> example-config
         st/configure
         (reset! state))
    (loader/load-resource "example" ["test/example/context.kn" "test/example/content.kn"])
    (jdbc/insert! @state "resources" example-resource)
    (f) ; run tests
    (reset! state saved-state)))
    ;(delete-test-db)))

(use-fixtures :once example-state-fixture)

(deftest test-states-loaded
  (->> "SELECT * FROM states LIMIT 10"
       st/query
       count
       (< 0)
       is))

(defn unpack-link
  [node]
  [(get-in node [:attrs :href]) (html/text node)])

(deftest test-resources-page
  (let [doc (html/html-snippet (:body (res/resources-page {})))]
    (->> (html/select doc [:.resource :a])
         (map unpack-link)
         (= [["/resources/all" "All Resources"]
             ["/resources/example" "Example"]
             ["http://example.com" "http://example.com"]])
         is)))

(deftest test-resource-page
  (let [doc (html/html-snippet (:body (res/resource-page {:params {:resource "example"}})))]
    (->> (html/select doc [:.resource :a])
         (map unpack-link)
         (= [["/resources/example/subjects" "Subjects"]
             ["/resources/example/predicates" "Predicates"]
             ["/resources/example/subject?curie=rdf:type" "type"]
             ["/resources/example/subject?curie=owl:Ontology" "owl:Ontology"]
             ["/resources/example/subject?curie=rdfs:comment" "rdfs:comment"]])
         is)))

(deftest test-subject-page
  (is (= "IRI	label	recognized	obsolete	replacement
https://example.com/0000111	barn owl primary remex feather	true		
"
         (:body
          (res/subject-page
           {:uri ""
            :params
            {:resource "example"
             "iri" "https://example.com/0000111"
             "format" "tsv"}}))))

  (is (= "@prefix owl: <http://www.w3.org/2002/07/owl#> .
@prefix knd: <https://knotation.org/datatype/> .
@prefix example: <https://ontology.iedb.org/ontology/ONTIE_> .
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix obo: <http://purl.obolibrary.org/obo/> .
@prefix kn: <https://knotation.org/> .
@prefix NCBITaxon: <http://purl.obolibrary.org/obo/NCBITaxon_> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
@prefix knp: <https://knotation.org/predicate/> .

<https://example.com/0000111>
  rdfs:label \"barn owl primary remex feather\" ;
  rdf:type owl:Class ;
  obo:IAO_0000115 \"A primary remex feather of a barn owl\" ;
  obo:IAO_0000118 \"grange hibou primaire remex plume\"@fr .
"
         (:body
          (res/subject-page
           {:uri ""
            :params
            {:resource "example"
             "iri" "https://example.com/0000111"
             "format" "ttl"}}))))

  (let [doc (html/html-snippet (:body (res/subject-page {:params {:resource "example" "iri" "https://example.com/0000111"}})))]
    (->> (html/select doc [:.subject :a])
         (map unpack-link)
         (= [["https://example.com/0000111" "https://example.com/0000111"]
             ["/resources/example" "example"]
             ["/resources/all/subject?iri=https%3A%2F%2Fexample.com%2F0000111"
              "Query all resources for this subject."]

             ["/resources/example/subject?curie=rdf:type" "type"]
             ["/resources/example/subject?curie=owl:Class" "owl:Class"]
             ["/resources/example/subject?curie=rdfs:label" "label"]
             ["/resources/example/subject?curie=obo:IAO_0000118" "alternative term"]
             ["/resources/example/subject?curie=obo:IAO_0000115" "definition"]

             ["/resources/example/subject?iri=https%3A%2F%2Fexample.com%2F0000111&format=ttl"
              "Turtle (ttl)"]
             ["/resources/example/subject?iri=https%3A%2F%2Fexample.com%2F0000111&format=json"
              "JSON-LD (json)"]
             ["/resources/example/subject?iri=https%3A%2F%2Fexample.com%2F0000111&format=tsv"
              "TSV (tsv)"]])
         is)
    (->> (html/select doc [:.subject :li :span html/text-node])
         (= ["barn owl primary remex feather"
             "grange hibou primaire remex plume"
             "A primary remex feather of a barn owl"])
         is)))

; /
; /ontology
; /ontology/ONTIE
; /ontology/ONTIE_0000001
; /resources
; /resources/all
; /resources/example
; /resources/example/subject
; /resources/example/subjects HTML TSV
; /resources/example/predicates

; /resources/all/subjects
