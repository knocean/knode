(ns com.knocean.knode.pages.ontology-test
  (:require [clojure.test :refer [deftest testing is]]
            [clojure.spec.alpha :as s]
            [clojure.string :as string]

            [cheshire.core :as json]

            [com.knocean.knode.pages.ontology.base :refer [ontology-result
                                                           parse-request-terms
                                                           parse-body-terms]]
            [com.knocean.knode.pages.ontology.tsv]
            [com.knocean.knode.pages.ontology.core]
            [com.knocean.knode.pages.mimetypes :as mime]))

;;;;;;;;;; Basic dummy data
(def env {:org.knotation.environment/prefix-iri {"rdf" "http://www.w3.org/1999/02/22-rdf-syntax-ns#", "rdfs" "http://www.w3.org/2000/01/rdf-schema#", "xsd" "http://www.w3.org/2001/XMLSchema#", "owl" "http://www.w3.org/2002/07/owl#", "obo" "http://purl.obolibrary.org/obo/", "knd" "https://knotation.org/datatype/", "knp" "https://knotation.org/predicate/", "ex" "https://example.com/"},
          :org.knotation.environment/iri-prefix {"http://www.w3.org/1999/02/22-rdf-syntax-ns#" "rdf", "http://www.w3.org/2000/01/rdf-schema#" "rdfs", "http://www.w3.org/2001/XMLSchema#" "xsd", "http://www.w3.org/2002/07/owl#" "owl", "http://purl.obolibrary.org/obo/" "obo", "https://knotation.org/datatype/" "knd", "https://knotation.org/predicate/" "knp", "https://example.com/" "ex"},
          :org.knotation.environment/prefix-seq ["rdf" "rdfs" "xsd" "owl" "obo" "knd" "knp" "ex"],
          :org.knotation.environment/label-iri {"part of" "http://purl.obolibrary.org/obo/BFO_0000050", "sample feather 33333" "https://example.com/0033333", "length (cm)" "https://example.com/0000002", "alternative term" "http://purl.obolibrary.org/obo/IAO_0000118", "default datatype" "https://knotation.org/predicate/default-datatype", "replacement" "http://purl.obolibrary.org/obo/IAO_0100001", "label" "http://www.w3.org/2000/01/rdf-schema#label", "primary remex feather" "http://purl.obolibrary.org/obo/UBERON_0011796", "replaced sample feather" "https://example.com/0033332", "obsolete" "http://www.w3.org/2002/07/owl#deprecated", "definition" "http://purl.obolibrary.org/obo/IAO_0000115", "in taxon" "http://purl.obolibrary.org/obo/RO_0002162", "link" "https://knotation.org/datatype/link", "OWL Manchester Syntax" "https://knotation.org/datatype/omn", "obsolete sample feather" "https://example.com/0033331", "type" "http://www.w3.org/1999/02/22-rdf-syntax-ns#type", "barn owl 2222" "https://example.com/0002222", "coloration" "https://example.com/0000003", "Tyto alba" "http://purl.obolibrary.org/obo/NCBITaxon_56313", "barn owl primary remex feather" "https://example.com/0000111", "birth date" "https://example.com/0000001", "subclass of" "http://www.w3.org/2000/01/rdf-schema#subClassOf"},
          :org.knotation.environment/iri-label {"http://purl.obolibrary.org/obo/BFO_0000050" "part of", "https://knotation.org/datatype/omn" "OWL Manchester Syntax", "https://example.com/0000001" "birth date", "http://purl.obolibrary.org/obo/UBERON_0011796" "primary remex feather", "https://knotation.org/predicate/default-datatype" "default datatype", "https://knotation.org/datatype/link" "link", "http://purl.obolibrary.org/obo/RO_0002162" "in taxon", "http://www.w3.org/1999/02/22-rdf-syntax-ns#type" "type", "https://example.com/0000111" "barn owl primary remex feather", "https://example.com/0033333" "sample feather 33333", "http://purl.obolibrary.org/obo/IAO_0000118" "alternative term", "http://purl.obolibrary.org/obo/IAO_0000115" "definition", "http://purl.obolibrary.org/obo/IAO_0100001" "replacement", "http://www.w3.org/2000/01/rdf-schema#subClassOf" "subclass of", "https://example.com/0000003" "coloration", "https://example.com/0033331" "obsolete sample feather", "https://example.com/0033332" "replaced sample feather", "http://purl.obolibrary.org/obo/NCBITaxon_56313" "Tyto alba", "https://example.com/0002222" "barn owl 2222", "http://www.w3.org/2002/07/owl#deprecated" "obsolete", "http://www.w3.org/2000/01/rdf-schema#label" "label", "https://example.com/0000002" "length (cm)"},
          :org.knotation.environment/label-seq ["label" "link" "OWL Manchester Syntax" "default datatype" "type" "subclass of" "replacement" "obsolete" "definition" "alternative term" "part of" "in taxon" "Tyto alba" "primary remex feather" "birth date" "length (cm)" "coloration" "barn owl primary remex feather" "barn owl 2222" "obsolete sample feather" "replaced sample feather" "sample feather 33333"],
          :org.knotation.environment/predicate-datatype {"https://knotation.org/predicate/default-datatype" "https://knotation.org/datatype/link", "http://www.w3.org/1999/02/22-rdf-syntax-ns#type" "https://knotation.org/datatype/link", "http://www.w3.org/2000/01/rdf-schema#subClassOf" "https://knotation.org/datatype/omn", "http://purl.obolibrary.org/obo/BFO_0000050" "https://knotation.org/datatype/link", "http://purl.obolibrary.org/obo/RO_0002162" "https://knotation.org/datatype/link", "https://example.com/0000001" "http://www.w3.org/2001/XMLSchema#date", "https://example.com/0000002" "http://www.w3.org/2001/XMLSchema#real"}}) ;; taken from example ontology project (might want to prune it?)

(def dummy-req {:env env :requested-iris [] :remaining-path []
                :headers {"accept" "text/html"}
                :params {} :form-params {} :query-params {} :uri "/ontology"})

;;;;;;;;;; Basic component specs
(s/def ::requested-iris vector?)
(s/def ::remaining-path vector?)
(s/def ::env map?)
(s/def ::headers map?)
(s/def ::params map?) (s/def ::form-params map?) (s/def ::query-params map?)
(s/def ::uri string?)
(s/def ::request (s/keys :req-un [::params ::form-params ::query-params ::uri ::headers]))
(s/def ::modified-request (s/merge ::request (s/keys :req-un [::env ::requested-iris ::remaining-path])))
(s/def ::response (s/keys :req-un [::status ::headers ::body]))

;;;;;;;;;; Ontolgy format tests
(s/def ::successful-response (s/and ::response #(= 200 (:status %))))

(s/fdef
 ontology-result
 :args ::modified-request
 :ret ::response)

(def ->result (mime/with-content-header ontology-result))

(defn basic-format-test [mimetype format-name success]
  (testing (str "Basic HTTP-level test for " format-name " requests")
    (let [success? (fn [& {:keys [] :as req}]
                     (is (s/valid? success (->result (merge dummy-req req)))))]
      (testing "with no requested IRIs"
        (testing (str "Specified via `accept` header returns " format-name " response")
          (success? :headers {"accept" mimetype}))
        (testing (str "Specified via format parameter header returns " format-name " response")
          (success? :headers {} :params {"format" format-name}))
        (testing (str "Specified via output-format parameter header returns " format-name " response")
          (success? :headers {} :params {"output-format" format-name}))
        (testing (str "Specified via file suffix returns " format-name " response")
          (success? :headers {} :params {} :uri (str "/ontology." format-name)))))
    (let [iri "https://example.com/0033333"
          success? (fn [& {:keys [] :as req}]
                     (let [res (->result (merge dummy-req req))]
                       (is (s/valid? success res))
                       (is (.contains (:body res) iri))))]
      (testing "with one requested IRI"
        (testing (str "Specified via `accept` header returns " format-name " response")
          (success? :headers {"accept" mimetype} :requested-iris [iri]))
        (testing (str "Specified via format parameter header returns " format-name " response")
          (success? :headers {} :params {"format" format-name} :requested-iris [iri]))
        (testing (str "Specified via output-format parameter header returns " format-name " response")
          (success? :headers {} :params {"output-format" format-name} :requested-iris [iri]))
        (testing (str "Specified via file suffix returns " format-name " response")
          (success? :headers {} :params {} :uri (str "/ontology." format-name) :requested-iris [iri]))))))

(s/def ::html-success
  (s/and
   ::successful-response
   #(= (get-in % [:headers "Content-Type"]) "text/html; charset=utf-8")))
;(deftest test-HTML-result-basics (basic-format-test "text/html" "html" ::html-success))

(s/def ::json-success
  (s/and ::successful-response
         #(= (get-in % [:headers "Content-Type"]) "application/json; charset=utf-8")
         #(not (empty? (:body %)))
         #(any? (json/decode (:body %)))))
(comment
  (deftest test-JSON-result-basics (basic-format-test "application/json" "json" ::json-success)))

(s/def ::tsv-success
  (s/and
   ::successful-response
   #(= (get-in % [:headers "Content-Type"]) "text/tab-separated-values; charset=utf-8")))

;(deftest test-TSV-result-basics (basic-format-test "text/tab-separated-values" "tsv" ::tsv-success))

(comment
  (deftest test-TSV-result-specifics
    (testing "the SELECT option can restrict/expand default fields in the result"
      (is (= "CURIE\tlabel\nex:0033333\tsample feather 33333"
             (:body
              (->result
               (assoc
                dummy-req :params {"format" "tsv" "select" "CURIE,label"}
                :requested-iris ["https://example.com/0033333"]))))))
    (testing "The compact=true option doesn't reduce the `label`  predicate to a `CURIE` in output"
      (is (= "CURIE\tlabel\nex:0033333\tsample feather 33333"
             (:body
              (->result
               (assoc
                dummy-req :params {"format" "tsv" "select" "CURIE,label" "compact" "true"}
                :requested-iris ["https://example.com/0033333"]))))))))

(comment
  (deftest test-TSV-multiple-results
    (testing "Baic multi-result level test for TSV format"
      (let [iris ["https://example.com/0033333" "https://example.com/0033332" "https://example.com/0033331"]
            success? (fn [& {:keys [] :as req}]
                       (let [res (->result (merge dummy-req req))]
                         (is (s/valid? ::tsv-success res))
                         (doseq [iri iris] (is (.contains (:body res) iri)))))]
        (testing (str "Specified via `accept` header returns tsv response")
          (success? :headers {"accept" "text/tab-separated-values"} :requested-iris iris))
        (testing (str "Specified via format parameter header returns tsv response")
          (success? :headers {} :params {"format" "tsv"} :requested-iris iris))
        (testing (str "Specified via output-format parameter header returns tsv response")
          (success? :headers {} :params {"output-format" "tsv"} :requested-iris iris))
        (testing (str "Specified via file suffix returns tsv response")
          (success? :headers {} :params {} :uri (str "/ontology." "tsv") :requested-iris iris))))))

(s/fdef parse-request-terms
        :args (s/cat :env ::env :req (s/keys :req-un [::params ::uri]))
        :ret seq?)

(comment
  (deftest test-term-parsing
    (testing "Parses main term"
      (testing "Gets main term from `iri` parameter"
        (is (= ["https://example.com/0033333"]
               (parse-request-terms
                env {:params {"iri" "https://example.com/0033333"}
                     :uri "/ontology"}))))
      (testing "Gets term from path var label or CURIE"
        (is (= ["https://example.com/0033333"]
               (parse-request-terms
                env {:params {} :uri "/ontology/ex:0033333"})))
        (is (= ["https://example.com/0033333"]
               (parse-request-terms
                env {:params {} :uri "/ontology/sample feather 33333"}))))
      (testing "Gives a higher priority to `iri` parameter")
      (is (= ["https://example.com/0033332"]
             (parse-request-terms
              env {:params {"iri" "https://example.com/0033332"}
                   :uri "/ontology/sample feather 33333"}))))

    (testing "Parses terms from parameters"
      (let [iris ["https://example.com/0033333" "https://example.com/0033332" "https://example.com/0033331"]
            curies ["ex:0033333" "ex:0033332" "ex:0033331"]]
        (testing "Gets terms from `CURIE` parameter"
          (is (= (take 1 iris)
                 (parse-request-terms
                  env {:params {"CURIE" (str "eq." (first curies))} :uri "/ontology"})))
          (is (= iris
                 (parse-request-terms
                  env {:params {"CURIE" (str "in." (string/join " " curies))} :uri "/ontology"}))))
        (testing "Gets terms from `IRI` parameter"
          (is (= (take 1 iris)
                 (parse-request-terms
                  env {:params {"IRI" (str "eq." (first iris))} :uri "/ontology"})))
          (is (= iris
                 (parse-request-terms
                  env {:params {"IRI" (str "in." (string/join " " iris))} :uri "/ontology"}))))))))

(s/fdef parse-body-terms
        :args (s/keys :req-un [::request-method ::params ::body])
        :ret seq?)

(comment
  (deftest test-parsing-body-terms
    (let [->body (fn [lines] (java.io.StringReader. (string/join \newline lines)))
          iris ["https://example.com/0033333" "https://example.com/0033332" "https://example.com/0033331"]
          curies ["ex:0033333" "ex:0033332" "ex:0033331"]]
      (testing "Parses terms from POST body"
        (testing "Only gets terms from POST body when the request is a POST with parameter `method` set to GET"
          (is (empty?
               (parse-body-terms
                {:params {}
                 :request-method :post
                 :body (->body (cons "IRI" iris))})))
          (is (empty?
               (parse-body-terms
                {:params {"method" "GET"}
                 :request-method :get
                 :body (->body (cons "IRI" iris))}))))
        (testing "Gets terms from TSV POST body"
          (is (= iris
                 (parse-body-terms
                  {:params {"method" "GET"}
                   :request-method :post
                   :body (->body (cons "IRI" iris))})))
          (is (= iris
                 (parse-body-terms
                  {:params {"method" "GET"}
                   :request-method :post
                   :body (->body (cons "IRI" iris))}))))))))
