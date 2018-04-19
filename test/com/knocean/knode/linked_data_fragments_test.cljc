(ns com.knocean.knode.linked-data-fragments-test
  (:require [clojure.test :refer [deftest testing is]]
            [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as gen]
            [clojure.string :as string]

            [com.knocean.knode.linked-data-fragments :as ldf]))

(s/def ::iri string?)
(s/def ::label string?)
(s/def ::language string?)
(s/def ::datatype string?)

(s/def ::object (s/keys :req-un [::iri] :opt-un [::label ::language ::datatype]))

(s/fdef ldf/string->object
        :args string?
        :ret ::object)

(deftest test-string->object
  (testing "Defaults to assuming IRI"
    (is (= {:iri "http://example.com/foo"} (ldf/string->object "http://example.com/foo")))
    (is (= {:iri "Foo"} (ldf/string->object "Foo"))))
  (testing "Deals with pointied IRIs"
    (is (= {:iri "http://example.com/foo"} (ldf/string->object "<http://example.com/foo>"))))
  (testing "Deals with quoted strings"
    (is (= {:label "Foo"} (ldf/string->object "\"Foo\"")))
    (is (= {:label "Foo" :language "en"} (ldf/string->object "\"Foo\"@en")))
    (is (= {:label "Foo" :datatype "http://example.com/string"}
           (ldf/string->object "\"Foo\"^^<http://example.com/string>")))))

(s/def ::graph string?) (s/def ::subject string?) (s/def ::predicate string?)
(s/def ::per-page integer?) (s/def ::pg integer?)
(s/def ::query (s/keys :req-un [::graph ::subject ::predicate ::object ::per-page ::pg]))

(deftest test-req->query)

(deftest test-matches-query?)
(deftest test-paginated)
