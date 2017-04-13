(ns knode.emit-test
  (:require [clojure.test :refer [deftest testing is]]
            [knode.emit :refer :all]))

(def context-blocks
  [{:prefix "ex" :iri "http://ex.com/"}])

(def subject
  {:subject {:iri "http://ex.com/foo" :curie "ex:foo"}})

(def blocks
  [{:predicate {:iri "http://ex.com/type" :curie "ex:type" :label "type"}
    :object {:iri "http://ex.com/bar" :curie "ex:bar" :label "Bar"}}
   {:predicate {:iri "http://ex.com/label" :curie "ex:label" :label "label"}
    :object {:lexical "Foo"}}
   {:predicate {:iri "http://ex.com/french" :curie "ex:french" :label "French"}
    :object {:lexical "Fou" :language "@fr"}}
   {:predicate {:iri "http://ex.com/obsolete" :curie "ex:obsolete" :label "obsolete"}
    :object
    {:lexical "true"
     :datatype {:iri "http://ex.com/boolean" :curie "ex:boolean"}}}])

(def example-ttl "@prefix ex: <http://ex.com/> .

ex:foo
  ex:type ex:bar
; ex:label \"Foo\"
; ex:french \"Fou\"@fr
; ex:obsolete \"true\"^^ex:boolean
.")

(def example-rdfa
  [:ul
   {:prefixes "prefix ex: http://ex.com/"
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
    [:a {:href "http://ex.com/french"} "French"]
    ": "
    [:span {:property "ex:french" :xml:lang "fr"} "Fou"]]
   [:li
    [:a {:href "http://ex.com/obsolete"} "obsolete"]
    ": "
    [:span {:property "ex:obsolete" :datatype "ex:boolean"} "true"]]])

(deftest test-emit
  (testing "Emit Turtle"
    (is (= (emit-ttl context-blocks subject blocks)
           example-ttl)))
  (testing "Emit HTML+RDFa"
    (is (= (emit-rdfa context-blocks subject blocks)
           example-rdfa))))
