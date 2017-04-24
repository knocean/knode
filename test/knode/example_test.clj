(ns knode.example-test
  (:require [clojure.test :refer [deftest testing is]]
            [clojure.string :as string]
            [clojure.data.json :as json]
            [knode.state :refer [state]]
            [knode.core :as core]
            [knode.emit :as emit]
            [knode.cli :as cli]
            [knode.server :as server]))

(def example-iri "https://example.com/ontology/EXAMPLE_0000002")

(def example-add-json
  {"name" "Foo"
   "alternative term" ["bar" "baz"]})

(def example-add-kn
  ": EXAMPLE:0000003
template: example class
name: Foo
alternative term: bar
alternative term: baz")

;; ## Setup

(def test-state
  {:root-dir "test/example/"
   :root-iri "https://example.com/"
   :dev-key "NOT SECRET"
   :project-name "example"
   :term-iri-format "https://example.com/ontology/EXAMPLE_%07d"})

(deftest test-example-ontology
  (swap! state merge test-state)
  (cli/load-state! "test/example/ontology/" "example")

  (testing "Load example ontology"
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
    (is (= (string/trim (emit/emit-index (:env @state) (:terms @state)))
           (string/trim (slurp "test/example/ontology/index.tsv"))))
    (is (= (emit/emit-ttl-terms (:env @state) (:context @state) (:terms @state))
           (string/trim (slurp "test/example/ontology/example.ttl"))))
    (is (= (emit/emit-kn-terms (:env @state) nil (:terms @state))
           (string/trim (slurp "test/example/ontology/example.kn")))))
  (testing "make example class"
    (let [{:keys [subject blocks] as :term}
          (server/make-term
           example-add-json
           (get-in @state [:templates "http://example.com/template-1"]))]
      (is (= (emit/emit-kn-term (:env @state) nil subject blocks))
          example-add-kn)))
  (testing "Unauthenticated"
    (is (= (:status (server/add-term! nil))
           401))
    (is (= (:status
            (server/add-term!
             {"template" "example class"
              "name" "Foo"}))
           403))
    (is (= (:status
            (server/add-term!
             {"api-key" "NOT THE RIGHT KEY"}))
           403)))
  (testing "Wrong template"
    (is (= (:status
            (server/add-term!
             {"api-key" "NOT SECRET"
              "template" "foo"}))
           400)))
  (testing "Missing required predicates"
    (is (= (:status
            (server/add-term!
             {"api-key" "NOT SECRET"
              "template" "example class"}))
           400)))
  (testing "Actually add a term"
    (with-redefs [spit :no-op]
      (let [result (server/add-term!
                    {"api-key" "NOT SECRET"
                     "template" "example class"
                     "name" "Foo"})
            body (json/read-str (:body result))]
        (is (= (:status result) 201))
        (is (nil? (:error result)))
        (is (= (get body "iri") "https://example.com/ontology/EXAMPLE_0000003"))
        (is (= (get body "curie") "EXAMPLE:0000003"))))))