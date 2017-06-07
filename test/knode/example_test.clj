(ns knode.example-test
  (:require [clojure.test :refer [deftest testing is]]
            [clojure.string :as string]
            [clojure.data.json :as json]
            [knode.state :refer [state]]
            [knode.core :as core]
            [knode.emit :as emit]
            [knode.cli :as cli]
            [knode.sparql :as sparql]

            [knode.server.server :as server]
            [knode.server.util :as sutil]))

(defn ex [x] (str "https://example.com/ontology/EXAMPLE_" x))
(def example-iri (ex "0000002"))

(def example-add-json
  {"template" "example class"
   "name" "Foo"
   "alternative term" ["bar" "baz"]})

(def example-add-kn
  ": EXAMPLE:0000005
template: example class
name: Foo
alternative term: bar
alternative term: baz
type: owl:Class
label: Example Foo")

;; ## Setup

(def test-state
  {:root-dir "test/example/"
   :root-iri "https://example.com/"
   :dev-key "NOT SECRET"})

(deftest test-example-ontology
  (reset! state (knode.state/init test-state))
  (cli/load-state! (:ontology-dir @state) (:project-name @state))
  (clojure.java.io/delete-file "tmp/blazegraph.jnl" true)
  (sparql/init-dataset! state)
  (sparql/load-terms! @state)

  (testing "Run validation"
    (is (= [] (sparql/validate @state))))

  (testing "Load example ontology"
    (is (= (:root-dir @state) "test/example/"))

    (is (= (->> (get-in @state [:terms example-iri :blocks])
                (map core/minimal-statement))
           [{:predicate {:iri "https://knotation.org/apply-template"}
             :object {:iri "http://example.com/template-1"}}
            {:predicate {:iri "https://example.com/ontology/EXAMPLE_name"}
             :object {:lexical "Two"}}
            {:predicate {:iri "http://www.w3.org/2000/01/rdf-schema#subClassOf"}
             :object {:iri (ex "0000001")}}
            {:template "http://example.com/template-1"
             :predicate {:iri "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"}
             :object {:iri "http://www.w3.org/2002/07/owl#Class"}}
            {:template "http://example.com/template-1"
             :predicate {:iri "http://www.w3.org/2000/01/rdf-schema#label"}
             :object {:lexical "Example Two"}}]))

    (is (= (string/trim (emit/emit-index (:env @state) (sutil/get-terms @state)))
           (->> "test/example/ontology/index.tsv"
                slurp
                string/split-lines
                (string/join \newline)
                (string/trim))))

    ;(is (= (emit/emit-ttl-terms (:env @state) (:context @state) (:terms @state))
    ;       (string/trim (slurp "test/example/ontology/example.ttl"))))

    (is (= (emit/emit-kn-terms (:env @state) nil (sutil/get-terms @state))
           (string/trim (slurp "test/example/ontology/example.kn"))))))

(deftest test-term-query
  (reset! state (knode.state/init test-state))
  (cli/load-state! (:ontology-dir @state) (:project-name @state))
  (clojure.java.io/delete-file "tmp/blazegraph.jnl" true)
  (sparql/init-dataset! state)
  (sparql/load-terms! @state)

  (testing "Returns the expected map for a present subject"
    (is (= (sparql/term-status @state (ex "0000001"))
           {:iri (ex "0000001")
            :recognized true
            :obsolete false
            :replacement nil})))
  (testing "Returns :recognized false for URIs that are not recorded anywhere"
    (is (= (sparql/term-status @state "nonexistent:iri")
           {:iri "nonexistent:iri"
            :recognized false
            :obsolete nil
            :replacement nil})))
  (testing "Returns :obsolete true for URIs marked obsolete"
    (is (= (sparql/term-status @state (ex "0000003"))
           {:iri (ex "0000003")
            :recognized true
            :obsolete true
            :replacement nil})))
  (testing "Returns a :replacement IRI for URIs marked obsolete AND replaced"
    (is (= (sparql/term-status @state (ex "0000004"))
           {:iri (ex "0000004")
            :recognized true
            :obsolete true
            :replacement (ex "0000002")})))

  (testing "More general queries"
    (is (= (sparql/query-predicates
            @state
            true
            [(ex "0000001")
             "FOO:BAR"]
            [["CURIE" "CURIE" nil]
             ["IRI" "IRI" nil]
             ["label" emit/rdfs:label nil]
             ["rep" "http://purl.obolibrary.org/obo/IAO_0000118" nil]])
           [[[{:curie "EXAMPLE:0000001" :iri (ex "0000001")}]
             [{:iri (ex "0000001")}]
             [{:lexical "Example One"}]
             [{:lexical "ex 1"} {:lexical "ex one"}]]
            [[{:curie "FOO:BAR" :iri "FOO:BAR"}]
             [{:iri "FOO:BAR"}]
             []
             []]]))

    (is (= (sparql/query-predicates
            @state
            true
            [(ex "0000002")]
            [["CURIE" "CURIE" nil]
             ["subclass of"
              "http://www.w3.org/2000/01/rdf-schema#subClassOf"
              nil]
             ["subclass of [label]"
              "http://www.w3.org/2000/01/rdf-schema#subClassOf"
              "label"]])
           [[[{:curie "EXAMPLE:0000002" :iri (ex "0000002")}]
             [{:curie "EXAMPLE:0000001" :iri (ex "0000001")}]
             [{:lexical "Example One"}]]])))

  (testing "More general queries as table"
    (is (= (sparql/query-predicates-tabular
            @state
            true
            [(ex "0000001")
             "FOO:BAR"]
            [["CURIE" "CURIE" nil]
             ["IRI" "IRI" nil]
             ["label" emit/rdfs:label nil]
             ["recognized" "recognized" nil]
             ["rep" "http://purl.obolibrary.org/obo/IAO_0000118" nil]])
           [["EXAMPLE:0000001" (ex "0000001") "Example One" "true" "ex 1|ex one"]
            ["FOO:BAR" "FOO:BAR" "" "false" ""]]))))
