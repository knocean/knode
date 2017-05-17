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
         {:iris [(ex "0000001")]
          :format nil}))
  (is (= (parse-ontology-request
          test-state
          {:route-params {:* "EXAMPLE_0000001.tsv"}})
         {:iris [(ex "0000001")]
          :format "tsv"}))
  (is (= (parse-ontology-request
          test-state
          {:route-params {:* "EXAMPLE_0000001"}
           :query-string "format=tsv"})
         {:iris [(ex "0000001")]
          :format "tsv"}))
  (is (= (parse-ontology-request
          test-state
          {:route-params {:* ""}
           :query-string (str "iris=" (ex "0000001") "&format=tsv")})
         {:iris [(ex "0000001")]
          :format "tsv"}))
  (is (= (parse-ontology-request
          test-state
          {:route-params {:* ""}
           :query-string "curies=EXAMPLE:0000001&format=tsv"})
         {:iris [(ex "0000001")]
          :format "tsv"}))
  (is (= (parse-ontology-request
          test-state
          {:route-params {:* ""}
           :query-string "format=tsv"
           :body "CURIE\nEXAMPLE:0000001"})
         {:iris [(ex "0000001")]
          :format "tsv"}))
  (is (= (parse-ontology-request
          test-state
          {:route-params {:* "EXAMPLE"}})
         {:iris [(ex "0000001")]
          :format nil})))
