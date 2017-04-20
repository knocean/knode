(ns knode.core-test
  (:require [clojure.test :refer [deftest testing is]]
            [knode.core :refer :all]))

(deftest test-parse-iri
  (testing "Parse some IRIs"
    (is (= (parse-iri "<http://foo.bar>")
           {:iri "http://foo.bar"}))
    (is (nil? (parse-iri "foo:bar")))
    (is (nil? (parse-iri "_:abc")))
    (is (nil? (parse-iri "abc")))))

(deftest test-parse-bnode
  (testing "Parse some blank nodes"
    (is (= (parse-bnode "_:abc")
           {:bnode "_:abc"}))
    (is (nil? (parse-bnode "<http://foo.bar>")))
    (is (nil? (parse-bnode "foo:bar")))
    (is (nil? (parse-bnode "abc")))))

(deftest test-parse-curie
  (testing "Parse some CURIEs"
    (is (= (parse-curie "foo:abc")
           {:curie "foo:abc"}))
    (is (nil? (parse-curie "<http://foo.bar>")))
    (is (nil? (parse-curie "_:abc")))
    (is (nil? (parse-curie "abc")))))

;; TODO: label parsing and tests
;; TODO: relative IRIs

(deftest test-parse-prefix-line
  (testing "Parse some @prefix lines"
    (is (= (parse-prefix-line "@prefix ex: <http://example.com/>")
           {:prefix "ex" :iri "http://example.com/"}))))

(deftest test-parse-label-line
  (testing "Parse some @label lines"
    (is (= (parse-label-line "@label type: rdf:type > link")
           {:label "type"
            :target {:name "rdf:type"}
            :datatype {:name "link"}}))))

(deftest test-parse-subject-line
  (testing "Parse some subject lines"
    (is (= (parse-subject-line ": ex:000001")
           {:subject {:name "ex:000001"}}))))

(deftest test-parse-statement-line
  (testing "Parse some statement lines"
    (is (= (parse-statement-line "type > link: owl:Class")
           {:predicate {:name "type"}
            :datatype {:name "link"}
            :content "owl:Class"}))))

;; Test processing

(def basic-env
  {:prefixes {"ex" "http://example.com/"}
   :labels
   {"type"
    {:iri "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"
     :datatype :link}}})

(deftest test-process-line
  (testing "Process some @prefix lines"
    (is (= (process-line {} "@prefix ex: <http://example.com/>")
           [{:prefixes {"ex" "http://example.com/"}}
            {:prefix "ex" :iri "http://example.com/"}])))
  (testing "Process some @label lines"
    (is (= (process-line
            {:prefixes {"rdf" "http://www.w3.org/1999/02/22-rdf-syntax-ns#"}}
            "@label type: rdf:type > link")
           [{:prefixes {"rdf" "http://www.w3.org/1999/02/22-rdf-syntax-ns#"}
             :labels
             {"type"
              {:iri "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"
               :datatype :link}}
             :iri-labels {"http://www.w3.org/1999/02/22-rdf-syntax-ns#type" "type"}}
            {:label "type"
             :target
             {:iri "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"
              :curie "rdf:type"
              :name "rdf:type"}
             :datatype :link}])))
  (testing "Process some subject lines"
    (is (= (process-line
            {:prefixes {"ex" "http://example.com/"}}
            ": ex:000001")
           [{:prefixes {"ex" "http://example.com/"}
             :subject {:iri "http://example.com/000001" :curie "ex:000001" :name "ex:000001"}}
            {:subject {:iri "http://example.com/000001" :curie "ex:000001" :name "ex:000001"}}])))
  (testing "Process some statement lines"
    (is (= (process-line basic-env "type > link: ex:000001")
           [basic-env
            {:predicate
             {:iri "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"
              :label "type"
              :name "type"}
             :content "ex:000001"
             :object {:iri "http://example.com/000001" :curie "ex:000001" :name "ex:000001"}}]))
    (is (= (process-line basic-env "type: ex:000001")
           [basic-env
            {:predicate
             {:iri "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"
              :label "type"
              :name "type"}
             :content "ex:000001"
             :object {:iri "http://example.com/000001" :curie "ex:000001" :name "ex:000001"}}]))
    (is (= (process-line basic-env "ex:comment: some text")
           [basic-env
            {:predicate
             {:iri "http://example.com/comment"
              :curie "ex:comment"
              :name "ex:comment"}
             :content "some text"
             :object {:lexical "some text"}}]))
    (is (= (process-line basic-env "ex:obsolete > ex:boolean: false")
           [basic-env
            {:predicate {:iri "http://example.com/obsolete" :curie "ex:obsolete" :name "ex:obsolete"}
             :content "false"
             :object {:lexical "false" :datatype {:iri "http://example.com/boolean" :curie "ex:boolean" :name "ex:boolean"}}}]))))
