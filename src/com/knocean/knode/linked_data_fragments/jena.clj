(ns com.knocean.knode.linked-data-fragments.jena
  (:require [clojure.java.jdbc :as sql])
  (:import (org.apache.jena.riot RDFDataMgr)
           (org.apache.jena.rdf.model ModelFactory)
           (org.apache.jena.query QueryFactory QueryExecutionFactory)
           (org.apache.jena.graph NodeFactory Triple)
           (org.apache.jena.graph.impl GraphBase)
           (org.apache.jena.sparql.core DatasetGraphBase Quad)))

(defn map->subj
  [{:keys [si sb]}]
  (or (and si (NodeFactory/createURI si))
      (and sb (NodeFactory/createBlankNode sb))))
(defn map->obj
  [{:keys [oi ob ol]}]
  (or (and oi (NodeFactory/createURI oi))
      (and ob (NodeFactory/createBlankNode ob))
      (and ol (NodeFactory/createLiteral ol))))

(defn map->triple
  [{:keys [pi] :as m}]
  (new Triple
       (map->subj m)
       (NodeFactory/createURI pi)
       (map->obj m)))

(defn map->quad
  [{:keys [gi pi] :as m}]
  (new Quad
       (NodeFactory/createURI gi)
       (map->subj m)
       (NodeFactory/createURI pi)
       (map->obj m)))

(defn seq->iterator
  [xs]
  (let [xs (atom xs)]
    (reify
      java.util.Iterator
      (hasNext [this] (not (empty? @xs)))
      (next [this] (let [x (first @xs)] (swap! xs rest) x))
      (remove [this]))))

(defn wrapped-iterator
  [xs]
  (org.apache.jena.util.iterator.WrappedIterator/create (seq->iterator xs)))

(defn make-triple
  []
  (map->triple
   {:si "http://example.com/s"
    :pi "http://example.com/p"
    :ol "1"}))

(defn make-quad
  []
  (map->quad
   {:gi "http://example.com/g"
    :si "http://example.com/s"
    :pi "http://example.com/p"
    :ol "1"}))

(def g
  (proxy [GraphBase] []
    ;; (find
    ;;   ([g s p o]
    ;;    (println "FOUR" g s p o)
    ;;    (wrapped-iterator [(make-quad)])))
    (graphBaseFind
      ([s p o]
       (println "THREE" s p o)
       (wrapped-iterator [(make-triple)])))))

(def m (ModelFactory/createModelForGraph g))
(def q (QueryFactory/create "select * where {?s <http://example.com/p> ?o ; ?p \"1\"}"))

(->> (QueryExecutionFactory/create q m)
     (.execSelect)
     iterator-seq
     first)
