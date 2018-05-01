(ns com.knocean.knode.jena
  (:require [clojure.java.jdbc :as sql])
  (:import (org.apache.jena.riot RDFDataMgr)
           (org.apache.jena.rdf.model ModelFactory)
           (org.apache.jena.query QueryFactory QueryExecutionFactory)
           (org.apache.jena.graph NodeFactory Triple)
           (org.apache.jena.graph.impl GraphBase)
           (org.apache.jena.sparql.core DatasetGraphBase Quad)))

(defn make-triple
  []
  (new Triple
       (NodeFactory/createURI "http://example.com/s")
       (NodeFactory/createURI "http://example.com/p")
       (NodeFactory/createLiteral "1")))

(defn make-quad
  []
  (new Quad
       (NodeFactory/createURI "http://example.com/g")
       (NodeFactory/createURI "http://example.com/s")
       (NodeFactory/createURI "http://example.com/p")
       (NodeFactory/createLiteral "1")))

(defn seq->iterator
  [xs]
  (let [xs (atom xs)]
    (reify
      java.util.Iterator
      (hasNext [this] (not (empty? @xs)))
      (next [this] (let [x (first @xs)] (swap! xs rest) x))
      (remove [this]))))

(def g
  (proxy [GraphBase] []
    (find
      ([g s p o]
       (println "FOUR" g s p o)
       (org.apache.jena.util.iterator.WrappedIterator/create
        (seq->iterator
         [(make-quad)]))))
    (graphBaseFind
      ([s p o]
       (println "THREE" s p o)
       (org.apache.jena.util.iterator.WrappedIterator/create
        (seq->iterator
         [(make-triple)]))))))

(def m (ModelFactory/createModelForGraph g))
(def q (QueryFactory/create "select * where {?s <http://example.com/p> ?o ; ?p \"1\"}"))

(->> (QueryExecutionFactory/create q m)
     (.execSelect)
     iterator-seq
     first)
