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
   :project-name "ex"
   :database-url "jdbc:sqlite:target/test.db"
   :base-iri "https://example.com/"})

(def example-resource
  {:label "ex"
   :title "Example"
   :description "Example description"
   :homepage "http://example.com"})

(defn delete-test-db
  []
  (try
    (fs/delete "target/test.db")
    (catch Exception e)))

(defn init-example-state
  []
  (delete-test-db)
  (->> example-config
       st/configure
       (reset! state))
  (loader/load-resource "ex" ["test/example/context.kn" "test/example/content.kn"])
  (jdbc/insert! @state "resources" example-resource))

(defn example-state-fixture
  [f]
  (let [saved-state @state]
    (init-example-state)
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

(defn select
  [selector doc]
  (html/select doc selector))

(defn unpack-link
  [node]
  [(get-in node [:attrs :href]) (html/text node)])

(deftest test-resources-page
  (let [doc
        (->> {}
             res/resources-page
             :body
             html/html-snippet)]
    (->> (html/select doc [:.resource :a])
         (map unpack-link)
         (= [["/resources/all" "All Resources"]
             ["/resources/ex" "Example"]
             ["http://example.com" "http://example.com"]])
         is)))

(deftest test-resource-page
  (let [doc
        (->> {:params {:resource "ex"}}
             res/resource-page
             :body
             html/html-snippet)]
    (->> (html/select doc [:.resource :a])
         (map unpack-link)
         (= [["/resources/ex/subjects" "Subjects"]
             ["/resources/ex/predicates" "Predicates"]
             ["/resources/ex/subject?curie=rdf:type" "type"]
             ["/resources/ex/subject?curie=owl:Ontology" "owl:Ontology"]
             ["/resources/ex/subject?curie=rdfs:comment" "rdfs:comment"]])
         is)))

(deftest test-subject-page
  (->> {:uri ""
        :params
        {:resource "ex"
         "iri" "https://example.com/0000111"
         "format" "tsv"}}
       res/subject-page
       :body
       (= "IRI	label	recognized	obsolete	replacement
https://example.com/0000111	barn owl primary remex feather	true		
")
       is)

  (->> {:uri ""
        :params
        {:resource "ex"
         "iri" "https://example.com/0000111"
         "format" "ttl"}}
       res/subject-page
       :body
       (= "@prefix owl: <http://www.w3.org/2002/07/owl#> .
@prefix knd: <https://knotation.org/datatype/> .
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix obo: <http://purl.obolibrary.org/obo/> .
@prefix kn: <https://knotation.org/> .
@prefix NCBITaxon: <http://purl.obolibrary.org/obo/NCBITaxon_> .
@prefix ex: <https://example.com/> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
@prefix knp: <https://knotation.org/predicate/> .

ex:0000111
  rdfs:label \"barn owl primary remex feather\" ;
  rdf:type owl:Class ;
  obo:IAO_0000115 \"A primary remex feather of a barn owl\" ;
  obo:IAO_0000118 \"grange hibou primaire remex plume\"@fr .
")
       is)

  (let [doc
        (->> {:params
              {:resource "ex"
               "iri" "https://example.com/0000111"}}
             res/subject-page
             :body
             html/html-snippet)]
    (->> (html/select doc [:.subject :a])
         (map unpack-link)
         (= [["https://example.com/0000111" "https://example.com/0000111"]
             ["/resources/ex" "ex"]
             ["/resources/all/subject?iri=https%3A%2F%2Fexample.com%2F0000111"
              "Query all resources for this subject."]

             ["/resources/ex/subject?curie=rdf:type" "type"]
             ["/resources/ex/subject?curie=owl:Class" "owl:Class"]
             ["/resources/ex/subject?curie=rdfs:label" "label"]
             ["/resources/ex/subject?curie=obo:IAO_0000118" "alternative term"]
             ["/resources/ex/subject?curie=obo:IAO_0000115" "definition"]

             ["/resources/ex/subject?curie=ex:0000111&format=ttl"
              "Turtle (ttl)"]
             ["/resources/ex/subject?curie=ex:0000111&format=json"
              "JSON-LD (json)"]
             ["/resources/ex/subject?curie=ex:0000111&format=tsv"
              "TSV (tsv)"]])
         is)
    (->> (html/select doc [:.subject :li :span html/text-node])
         (= ["barn owl primary remex feather"
             "grange hibou primaire remex plume"
             "A primary remex feather of a barn owl"])
         is)))

(deftest test-predicates-page
  (->> {:uri ""
        :params
        {:resource "ex"
         "format" "tsv"
         "compact" "true"}}
       res/predicates-page
       :body
       (= "CURIE	label	obsolete	replacement
obo:BFO_0000050	part of		
obo:IAO_0000115	definition		
obo:IAO_0000118	alternative term		
obo:IAO_0100001	replacement		
rdf:type	type		
rdfs:comment			
rdfs:label	label		
owl:deprecated	obsolete		
ex:0000001	birth date		
ex:0000002	length (cm)		
knp:applied-template			
knp:default-datatype	default datatype		
knp:template-content			
")
       is))

(deftest test-subjects-page
  (->> {:uri ""
        :params
        {:resource "ex"
         "format" "tsv"
         "compact" "true"
         "limit" "5"
         "offset" "12"}}
       res/subjects-page
       :body
       (= "CURIE	label	obsolete	replacement
ex:0000001	birth date		
ex:0000002	length (cm)		
ex:0000003	coloration		
ex:0000111	barn owl primary remex feather		
ex:0002222	barn owl 2222		
")
       is))

; /
; /ontology
; /ontology/ONTIE
; /ontology/ONTIE_0000001
; /resources
; /resources/all
; /resources/ex
; /resources/ex/subject
; /resources/ex/subjects HTML TSV
; /resources/ex/predicates

; /resources/all/subjects
