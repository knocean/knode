(ns com.knocean.knode.linked-data-fragments-test
  (:require [clojure.test :refer [deftest testing is]]
            [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as gen]
            [clojure.string :as string]
            [clojure.set :as set]

            [org.knotation.rdf :as rdf]

            [com.knocean.knode.linked-data-fragments.base :as base]
            [com.knocean.knode.linked-data-fragments.core :as ldf]))

(s/def ::full-string (s/and string? #(> (count %) 0)))

(s/def ::oi ::full-string)
(s/def ::ol ::full-string)
(s/def ::ln ::full-string)
(s/def ::di ::full-string)

(s/def ::object
  (s/or :iri (s/keys :req-un [::oi])
        :parsed (s/keys :req-un [::oi ::ol] :opt-un [::ln ::di])))

(s/fdef ldf/string->object
        :args string?
        :ret ::object)

(comment
  (deftest test-string->object
    (testing "Defaults to assuming IRI"
      (is (= {:oi "http://example.com/foo"} (ldf/string->object "http://example.com/foo")))
      (is (= {:oi "Foo"} (ldf/string->object "Foo"))))
    (testing "Deals with pointied IRIs"
      (is (= {:oi "http://example.com/foo"} (ldf/string->object "<http://example.com/foo>"))))
    (testing "Deals with quoted strings"
      (is (= {:ol "Foo"} (ldf/string->object "\"Foo\"")))
      (is (= {:ol "Foo" :ln "en"} (ldf/string->object "\"Foo\"@en")))
      (is (= {:ol "Foo" :di "http://example.com/string"}
             (ldf/string->object "\"Foo\"^^<http://example.com/string>"))))))

(s/def ::gi ::full-string) (s/def ::si ::full-string) (s/def ::pi ::full-string)
(s/def ::sb ::full-string) (s/def ::ob ::full-string)
(s/def ::per-page (s/and integer? #(> % 0)))
(s/def ::page (s/and integer? #(>= % 0)))
(s/def ::query (s/keys :req-un [::per-page ::page]
                       :opt-un [::gi ::si ::pi ::oi ::ol ::ln ::di]))

(s/fdef ldf/req->query
        :args map?
        :ret ::query)

(deftest test-req->query
  (testing "Defaults to 100 per-page, page 0"
    (is (= {:per-page base/+per-page+ :page 0} (ldf/req->query nil))))
  (testing "When provided :params"
    (testing "Takes the value of graph, subject, predicate at face value, gets the numeric value of per-page and page"
      (is (= {:gi "foo" :si "bar" :pi "baz"
              :per-page 37 :page 12}
             (ldf/req->query {:params {"graph" "foo" "subject" "bar" "predicate" "baz" "per-page" "37" "page" "12"}}))))
    (testing "Doesn't take junk in the :per-page or :page slots"
      (doseq [junk (gen/sample (s/gen string?))]
        (let [res (ldf/req->query {:params {"per-page" junk "page" junk}})]
          (is (and (integer? (:per-page res))
                   (integer? (:page res)))))))
    (testing "Disregards arbitrary other keys"
      (doseq [junk (gen/sample (s/gen map?))]
        (is (set/subset?
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
                   "page" "12"})})))
             #{:gi :si :pi :oi :ol :ln :di :per-page :page}))))))

(s/def ::entry
  (s/or :default (s/keys :req-un [::gi ::si ::pi ::oi]
                         :opt-un [::ln ::di])
        :blank-sub (s/keys :req-un [::gi ::sb ::pi ::oi]
                           :opt-un [::ln ::di])
        :label-obj (s/keys :req-un [::gi ::si ::pi ::ol]
                           :opt-un [::ln ::di])
        :blank-obj (s/keys :req-un [::gi ::si ::pi ::oi]
                           :opt-un [::sb ::ob ::ol ::ln ::di])))

(deftest test-matches-query?
  (testing "Nonexistent slots match anything"
    (doseq [e (gen/sample (s/gen ::entry))]
      (is (ldf/matches-query? {} e))))
  (testing "Slots with values match literally"
    (is (ldf/matches-query?
         {:gi "foo" :si "bar"}
         {:gi "foo" :si "bar" :pi "baz" :oi "mumble"})))
  (testing "Supports blank node queries"
    (is (ldf/matches-query?
         {:oi ":blank"}
         {:gi "foo" :si "bar" :pi "baz" :ob "_:mumble"}))
    (is (not
         (ldf/matches-query?
          {:oi ":blank"}
          {:gi "foo" :si "bar" :pi "baz" :oi "mumble"})))))

(s/def ::total (s/and integer? #(> % 0)))
(s/def ::per-page (s/and integer? #(> % 0)))
(s/def ::page (s/and integer? #(> % 0)))
(s/def ::items coll?)
(s/def ::paginated (s/keys :req-un [::total ::per-page ::page ::items]))

(s/fdef ldf/paginated
        :args (s/cat :per-page ::per-page :page ::page :seq ::items)
        :ret ::object)

(deftest test-paginated
  (testing "If there are enough items to fill out the page, show them"
    (is (= {:total 10, :per-page 10, :page 0, :items [1 2 3 4 5 6 7 8 9 10]}
           (ldf/paginated 10 0 [1 2 3 4 5 6 7 8 9 10]))))
  (testing "If there are not enough items to fill out a page, show a full page"
    (is (= {:total 12, :per-page 10, :page 0, :items [1 2 3 4 5 6 7 8 9 10]}
           (ldf/paginated 10 0 [1 2 3 4 5 6 7 8 9 10 11 12]))))
  (testing "If there are fewer items than a page-worth, show partial page"
    (is (= {:total 5, :per-page 10, :page 0, :items [1 2 3 4 5]}
           (ldf/paginated 10 0 [1 2 3 4 5]))))
  (testing "If we paginate past the first page, drop the appropriate number of items"
    (is (= {:total 12, :per-page 5, :page 1, :items [6 7 8 9 10]}
           (ldf/paginated 5 1 [1 2 3 4 5 6 7 8 9 10 11 12])))))
