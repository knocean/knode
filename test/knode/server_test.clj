(ns knode.server-test
  (:require [clojure.test :refer [deftest testing is]]
            [knode.server :refer :all]))

(defn ex [x] (str "https://example.com/ontology/EXAMPLE_" x))

(def test-state
  {:root-dir "test/example/"
   :root-iri "https://example.com/"
   :idspace "EXAMPLE"
   :env {:prefixes {"EXAMPLE" "https://example.com/ontology/EXAMPLE_"}}
   :terms {(ex "0000001") {}}})

(deftest test-parse-request
  (is (= (parse-ontology-request
          test-state
          {:route-params {:* "EXAMPLE_0000001"}})
         {:style "IRI"
          :iri (ex "0000001")}))
  (is (= (parse-ontology-request
          test-state
          {:route-params {:* "EXAMPLE_0000001.tsv"}})
         {:format "tsv"
          :style "IRI"
          :iri (ex "0000001")}))
  (is (= (parse-ontology-request
          test-state
          {:route-params {:* "EXAMPLE_0000001"}
           :query-string "format=tsv"})
         {:format "tsv"
          :style "IRI"
          :iri (ex "0000001")}))
  (is (= (parse-ontology-request
          test-state
          {:route-params {:* ""}
           :query-string (str "IRI=eq." (ex "0000001") "&format=tsv")})
         {:format "tsv"
          :style "IRI"
          :iri (ex "0000001")}))
  (is (= (parse-ontology-request
          test-state
          {:route-params {:* ""}
           :query-string (str "IRI=in." (ex "0000001") "&format=tsv")})
         {:format "tsv"
          :style "IRI"
          :iris [(ex "0000001")]}))
  (is (= (parse-ontology-request
          test-state
          {:route-params {:* ""}
           :query-string "CURIE=EXAMPLE:0000001"})
         {:error "CURIE query should start with 'eq.' or 'in.', not: CURIE=EXAMPLE:0000001"}))
  (is (= (parse-ontology-request
          test-state
          {:route-params {:* ""}
           :query-string "CURIE=eq.FOO:BAR"})
         {:style "CURIE"
          :iri "FOO:BAR"}))
  (is (= (parse-ontology-request
          test-state
          {:route-params {:* ""}
           :query-string "CURIE=in.EXAMPLE:0000001&format=tsv"})
         {:format "tsv"
          :style "CURIE"
          :iris [(ex "0000001")]}))
  (is (= (parse-ontology-request
          test-state
          {:route-params {:* ""}
           :query-string "format=tsv"
           :body "CURIE\nEXAMPLE:0000001"})
         {:format "tsv"
          :style "CURIE"
          :iris [(ex "0000001")]}))
  (is (= (parse-ontology-request
          test-state
          {:route-params {:* "EXAMPLE"}})
         {:style "IRI"
          :iris [(ex "0000001")]}))
  (is (= (parse-ontology-request
          test-state
          {:route-params {:* ""}})
         {:style "IRI"
          :iris [(ex "0000001")]})))
