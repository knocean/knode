(ns knode.example-test
  (:require [clojure.test :refer [deftest testing is]]
            [clojure.string :as string]
            [clojure.data.json :as json]
            [knode.state :as state :refer [state]]
            [knode.core :as core]
            [knode.emit :as emit]
            [knode.cli :as cli]
            [knode.server :as server]
            [knode.sparql :as sparql]))

(def example-iri "https://example.com/ontology/EXAMPLE_0000002")

(def example-add-json
  {"name" "Foo"
   "alternative term" ["bar" "baz"]})

(def example-add-kn
  ": EXAMPLE:0000003
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

(defn reset-state! []
  (reset! state (knode.state/init test-state))
  (cli/load-state! "test/example/ontology/" "example")
  (sparql/init-dataset! state)
  (sparql/load-terms! @state))

(deftest test-example-ontology
  (reset-state!)
  (is (= [] (sparql/validate @state)))

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
    ;(is (= (emit/emit-ttl-terms (:env @state) (:context @state) (:terms @state))
    ;       (string/trim (slurp "test/example/ontology/example.ttl"))))
    (is (= (emit/emit-kn-terms (:env @state) nil (:terms @state))
           (string/trim (slurp "test/example/ontology/example.kn")))))
  (testing "make example class"
    (let [{:keys [subject blocks] as :term}
          (server/make-term
           (get-in @state [:templates "http://example.com/template-1"])
           example-add-json)]
      (is (= (emit/emit-kn-term (:env @state) nil subject blocks)
             example-add-kn))))
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

(deftest test-term-status
  (reset-state!)

  (testing "Returns the expected map for a present subject"
    (is (= {:CURIE "EXAMPLE:0000001" :recognized true :obsolete false :replacement nil}
           (state/term-status "EXAMPLE:0000001" :label :CURIE))))
  (testing "Returns :recognized false for URIs that are not recorded anywhere"
    (is (= {:CURIE "nonexistent:iri" :recognized false :obsolete false :replacement nil}
           (state/term-status "nonexistent:iri" :label :CURIE))))
  (testing "Returns :obsolete true for URIs marked obsolete"
    (is (= {:CURIE "EXAMPLE:0000003" :recognized true :obsolete true :replacement nil}
           (state/term-status "EXAMPLE:0000003" :label :CURIE))))
  (testing "Returns a :replacement IRI for URIs marked obsolete AND replaced"
    (is (= {:CURIE "EXAMPLE:0000004" :recognized true :obsolete true :replacement "EXAMPLE:0000002"}
           (state/term-status "EXAMPLE:0000004" :label :CURIE))))
  (testing "Checks blazegraph for IRIs not found in (:terms @state)"
    :TODO))
