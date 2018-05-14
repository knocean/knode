(ns com.knocean.knode.linked-data-fragments.jena
  (:require [com.knocean.knode.linked-data-fragments.sql :as sql]

            [clojure.reflect :as r]
            [clojure.pprint :as pp]

            [com.knocean.knode.state :refer [state] :as st]
            [com.knocean.knode.linked-data-fragments.base :as base :refer [query query-stream]])
  (:import (org.apache.jena.riot RDFDataMgr)
           (org.apache.jena.rdf.model ModelFactory)
           (org.apache.jena.query QueryFactory QueryExecutionFactory)
           (org.apache.jena.graph NodeFactory Triple)
           (org.apache.jena.graph.impl GraphBase)
           (org.apache.jena.sparql.core DatasetGraphBase Quad)
           (org.apache.jena.query DatasetFactory)))

(defn reflect [thing]
  (pp/print-table (sort-by :name (filter :exception-types (:members (r/reflect thing))))))

(defn map->subj
  [{:keys [si sb]}]
  (or (and si (NodeFactory/createURI si))
      (and sb (NodeFactory/createBlankNode sb))))
(defn map->obj
  [{:keys [oi ob ol ln]}]
  (or (and oi (NodeFactory/createURI oi))
      (and ob (NodeFactory/createBlankNode ob))
      (and ol ln (NodeFactory/createLiteral ol ln))
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

(defn node-type
  [thing]
  (let [c (class thing)]
    (cond ;; NOTE - case DOES NOT work here, despite the equivalence comparison
      (= c org.apache.jena.graph.Node_ANY) :any
      (= c org.apache.jena.graph.Node_Blank) :blank
      (= c org.apache.jena.graph.Node_URI) :uri
      (= c org.apache.jena.graph.Node_Literal) :literal)))

(defn ->query-val
  [thing]
  (case (node-type thing)
    :any nil
    :blank :blank
    :uri (.toString thing)))

(defn ->query-obj
  [thing]
  ;; TODO FIXME
  (case (node-type thing)
    :literal {:ol (.getLiteralLexicalForm thing)
              :ln (let [ln (.getLiteralLanguage thing)] (if (empty? ln) nil ln))
              ;; It look like this returns http://www.w3.org/2001/XMLSchema#string
              ;; whether the original input to Node_Literal had a datatype or not.
              ;; I guess that means #string needs to match either itself or the
              ;; empty datatype?
              :di (.getLiteralDatatypeURI thing)}
    :uri {:oi (->query-val thing)}
    :blank {:ob (->query-val thing)}
    :any nil))

(defn spo->query
  [s p o]
  (into {} (filter second (merge
                           (if (= :uri (node-type s))
                             {:si (->query-val s)}
                             {:sb (->query-val s)})
                           {:pi (->query-val p)}
                           (->query-obj o)))))

(defn gspo->query
  [g s p o]
  (merge
   {:gi (->query-val g)}
   (spo->query s p o)))

(defn graph [source]
  (proxy [GraphBase] []
    (graphBaseFind
      ([s p o]
       (wrapped-iterator
        (map map->triple
             (query-stream
              (spo->query s p o)
              source)))))))

(defn dataset-graph [source]
  (proxy [DatasetGraphBase] []
    ;; NOTE - it looks like this is never actually used; the queries run against
    ;;        whatever is returned by getDefaultGraph (even when specifying GRAPH)
    (find
      ([g s p o]
       (println "FINDING...")
       (wrapped-iterator
        (map map->quad)
        (query-stream
         (gspo->query s p o)
         source))))
    
    (getGraph ([] (graph source)))
    (getDefaultGraph ([] (graph source)))

    ;; NOTE - leaving this out causes errors when calling DatasetFactory/wrap on this proxy
    ;;        based on
    ;;          https://jena.apache.org/documentation/javadoc/arq/org/apache/jena/sparql/core/DatasetGraph.html#supportsTransactions--
    ;;        it looks like we shouldn't have to worry about returning false for read-only datasets
    (supportsTransactions ([] false))))

(defn model [graph]
  (DatasetFactory/wrap graph))

(def source #(:maps @st/state))

(def g (atom (dataset-graph (source))))
(def m (atom (model @g)))

(do
  (add-watch
   st/state :jena-base-recompute
   (fn [k atom old new]
     (when (= k :jena-base-recompute)
       (println "Recomputing Jena graph on @state change...")
       (reset! g (graph (source)))
       (reset! m (model @g)))
     nil))
  nil)

(defn query-jena [sparql-string]
  (->> (QueryExecutionFactory/create (QueryFactory/create sparql-string) @m)
       (.execSelect)
       iterator-seq
       first))

(query-jena "select * where {?s <http://example.com/p> ?o ; ?p \"1\"}")
(query-jena "select * where {?s <http://protege.stanford.edu/plugins/owl/protege#defaultLanguage> ?o }")
(query-jena "select ?g ?s ?p ?o where { {?s ?p ?o} union { GRAPH ?g {?s ?p ?o} } }")
