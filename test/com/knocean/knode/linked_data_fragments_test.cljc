(ns com.knocean.knode.linked-data-fragments-test
  (:require [clojure.test :refer [deftest testing is]]
            [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as gen]
            [clojure.string :as string]

            [org.knotation.object :as ob]
            [org.knotation.rdf :as rdf]

            [com.knocean.knode.linked-data-fragments :as ldf]))

(s/def ::full-string (s/and string? #(> (count %) 0)))

(s/def ::iri ::full-string)
(s/def ::label ::full-string)
(s/def ::language ::full-string)
(s/def ::datatype ::full-string)

(s/def ::object
  (s/or :iri (s/keys :req-un [::iri])
        :parsed (s/keys :req-un [::iri ::label] :opt-un [::language ::datatype])))

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

(s/def ::graph ::full-string)
(s/def ::subject ::full-string)
(s/def ::predicate ::full-string)
(s/def ::per-page (s/and integer? #(> % 0)))
(s/def ::pg (s/and integer? #(>= % 0)))
(s/def ::query (s/keys :req-un [::per-page ::pg]
                       :opt-un [::graph ::subject ::predicate ::object]))

(s/fdef ldf/req->query
        :args map?
        :ret ::query)

(deftest test-req->query
  (testing "Defaults to 100 per-page, page 0, with nils everywhere else"
    (is (= {:object nil :per-page 100 :pg 0} (ldf/req->query nil))))
  (testing "When provided :params"
    (testing "Takes the value of graph, subject, predicate at face value, gets the numeric value of per-page and pg"
      (is (= {:graph "foo" :subject "bar" :predicate "baz" :object nil
              :per-page 37 :pg 12}
             (ldf/req->query {:params {"graph" "foo" "subject" "bar" "predicate" "baz" "per-page" "37" "pg" "12"}}))))
    (testing "Doesn't take junk in the :per-page or :pg slots"
      (doseq [junk (gen/sample (s/gen string?))]
        (let [res (ldf/req->query {:params {"per-page" junk "pg" junk}})]
          (is (and (integer? (:per-page res))
                   (integer? (:pg res)))))))
    (testing "Disregards arbitrary other keys"
      (doseq [junk (gen/sample (s/gen map?))]
        (is (= #{:graph :subject :predicate :object :per-page :pg}
               (set
                (keys
                 (ldf/req->query
                  {:params
                   (merge
                    junk
                    {"graph" "foo"
                     "subject" "bar"
                     "predicate" "baz"
                     "per-page" "37"
                     "pg" "12"})})))))))))

(deftest test-matches-query?)
(deftest test-paginated)
