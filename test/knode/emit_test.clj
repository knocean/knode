(ns knode.emit-test
  (:require [clojure.test :refer [deftest testing is]]
            [knode.core :as core]
            [knode.emit :refer :all]))

(def context-blocks
  [{:prefix "ex" :iri "http://ex.com/"}
   {:label "type" :target {:iri "http://ex.com/type"} :datatype :link}
   {:label "label" :target {:iri "http://ex.com/label"}}
   {:label "boolean" :target {:iri "http://ex.com/boolean"}}
   {:label "obsolete"
    :target {:iri "http://ex.com/obsolete"}
    :datatype {:iri "http://ex.com/boolean"}}
   {:label "Bar" :target {:iri "http://ex.com/bar"}}
   {:label "French"
    :target {:iri "http://ex.com/french"}
    :datatype {:language "@fr"}}])

(def environment
  (reduce core/update-environment {} context-blocks))

(def subject {:iri "http://ex.com/foo"})

(def blocks
  [{:predicate {:iri "http://ex.com/type"}
    :object {:iri "http://ex.com/bar"}}
   {:predicate {:iri "http://ex.com/label"}
    :object {:lexical "Foo"}}
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
    [:a {:href "http://ex.com/french"} "French"]
    ": "
    [:span {:property "ex:french" :xml:lang "fr"} "Fou"]]
   [:li
    [:a {:href "http://ex.com/obsolete"} "obsolete"]
    ": "
    [:span {:property "ex:obsolete" :datatype "ex:boolean"} "true"]]])

(def example-kn "@prefix ex: <http://ex.com/>
@label type: ex:type > link
@label label: ex:label
@label boolean: ex:boolean
@label obsolete: ex:obsolete > ex:boolean
@label Bar: ex:bar
@label French: ex:french > @fr

: ex:foo
type: Bar
label: Foo
French: Fou
obsolete: true")

(deftest test-emit
  (testing "Emit Turtle"
    (is (= (emit-ttl environment context-blocks subject blocks)
           example-ttl)))
  (testing "Emit HTML+RDFa"
    (is (= (emit-rdfa environment context-blocks subject blocks)
           example-rdfa)))
  (testing "Emit Knotation"
    (is (= (emit-kn environment context-blocks subject blocks)
           example-kn))))
