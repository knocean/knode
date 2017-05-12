(ns knode.emit-test
  (:require [clojure.test :refer [deftest testing is]]
            [clojure.data.json :as json]
            [knode.core :as core]
            [knode.emit :refer :all]))

(def context-blocks
  [{:prefix "ex" :iri "http://ex.com/"}
   {:label "type"
    :target {:iri "http://ex.com/type"}
    :datatype :link
    :cardinality "one or more"}
   {:label "label"
    :target {:iri "http://ex.com/label"}
    :cardinality "one"}
   {:label "synonym"
    :target {:iri "http://ex.com/synonym"}
    :cardinality "zero or more"}
   {:label "boolean" :target {:iri "http://ex.com/boolean"}}
   {:label "obsolete"
    :target {:iri "http://ex.com/obsolete"}
    :datatype {:iri "http://ex.com/boolean"}
    :cardinality "one"}
   {:label "Bar" :target {:iri "http://ex.com/bar"}}
   {:label "French"
    :target {:iri "http://ex.com/french"}
    :datatype {:language "@fr"}
    :cardinality "one"}])

(def environment
  (reduce core/update-environment {} context-blocks))

(def subject {:iri "http://ex.com/foo"})

(def blocks
  [{:predicate {:iri "http://ex.com/type"}
    :object {:iri "http://ex.com/bar"}}
   {:predicate {:iri "http://ex.com/label"}
    :object {:lexical "Foo"}}
   {:predicate {:iri "http://ex.com/synonym"}
    :object {:lexical "foo"}}
   {:predicate {:iri "http://ex.com/french"}
    :object {:lexical "Fou" :language "@fr"}}
   {:predicate {:iri "http://ex.com/obsolete"}
    :object
    {:lexical "true"
     :datatype {:iri "http://ex.com/boolean"}}}])

(def example-ttl "@prefix ex: <http://ex.com/> .

ex:foo
  ex:type ex:bar
; ex:label \"Foo\"
; ex:synonym \"foo\"
; ex:french \"Fou\"@fr
; ex:obsolete \"true\"^^ex:boolean
.")

(def example-rdfa
  [:ul
   {:prefix "ex: http://ex.com/"
    :resource "ex:foo"}
   [:li
    [:a {:href "http://ex.com/type"} "type"]
    ": "
    [:a {:href "http://ex.com/bar" :property "ex:type"} "Bar"]]
   [:li
    [:a {:href "http://ex.com/label"} "label"]
    ": "
    [:span {:property "ex:label"} "Foo"]]
   [:li
    [:a {:href "http://ex.com/synonym"} "synonym"]
    ": "
    [:span {:property "ex:synonym"} "foo"]]
   [:li
    [:a {:href "http://ex.com/french"} "French"]
    ": "
    [:span {:property "ex:french" :xml:lang "fr"} "Fou"]]
   [:li
    [:a {:href "http://ex.com/obsolete"} "obsolete"]
    ": "
    [:span {:property "ex:obsolete" :datatype "ex:boolean"} "true"]]])

(def example-jsonld
  {"@context"
   {"ex" "http://ex.com/"
    "type" {"@id" "ex:type" "iri" "http://ex.com/type"}
    "label" {"@id" "ex:label" "iri" "http://ex.com/label"}
    "synonym" {"@id" "ex:synonym" "iri" "http://ex.com/synonym"}
    "obsolete" {"@id" "ex:obsolete" "iri" "http://ex.com/obsolete"}
    "boolean" {"@id" "ex:boolean" "iri" "http://ex.com/boolean"}
    "Bar" {"@id" "ex:bar" "iri" "http://ex.com/bar"}
    "French" {"@id" "ex:french" "iri" "http://ex.com/french"}}
   "@id" "ex:foo"
   "iri" "http://ex.com/foo"
   "curie" "ex:foo"
   "type" [{"@id" "ex:bar" "iri" "http://ex.com/bar" "label" "Bar"}]
   "label" {"@value" "Foo"}
   "synonym" [{"@value" "foo"}]
   "French" {"@value" "Fou" "@language" "fr"}
   "obsolete" {"@value" "true" "@type" "ex:boolean"}})

(def example-kn "@prefix ex: <http://ex.com/>
@label type: ex:type > link
@label label: ex:label
@label synonym: ex:synonym
@label boolean: ex:boolean
@label obsolete: ex:obsolete > ex:boolean
@label Bar: ex:bar
@label French: ex:french > @fr

: ex:foo
type: Bar
label: Foo
synonym: foo
French: Fou
obsolete: true")

(deftest test-emit
  (testing "Emit Turtle"
    (is (= (emit-ttl-term environment context-blocks subject blocks)
           example-ttl)))
  (testing "Emit HTML+RDFa"
    (is (= (emit-rdfa-term environment context-blocks subject blocks)
           example-rdfa)))
  (testing "Emit JSON-LD"
    (is (= (emit-jsonld-term environment context-blocks subject blocks)
           example-jsonld)))
  (testing "Emit Knotation"
    (is (= (emit-kn-term environment context-blocks subject blocks)
           example-kn))))
