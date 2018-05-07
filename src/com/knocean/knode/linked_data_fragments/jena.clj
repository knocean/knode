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
           (org.apache.jena.sparql.core DatasetGraphBase Quad)))

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

;; (def gs
;;   (proxy [DatasetGraphBase] []
;;     (find
;;       ([g s p o]
;;        (println "FOUR" g s p o)
;;        (wrapped-iterator [(make-quad)])))))

(defn ->query-val
  [thing]
  (let [c (class thing)]
    (cond ;; case DOES NOT work here, despite the equivalence comparison
      (= c org.apache.jena.graph.Node_ANY) nil
      (= c org.apache.jena.graph.Node_Blank) :blank
      (= c org.apache.jena.graph.Node_URI) (.toString thing))))

(defn ->query-obj
  [thing]
  ;; TODO
  (if (= (class thing) org.apache.jena.graph.Node_Literal)
    {:o (.getLiteralLexicalForm thing)
     :ln (let [ln (.getLiteralLanguage thing)] (if (empty? ln) nil ln))
     ;; It look like this returns http://www.w3.org/2001/XMLSchema#string
     ;; whether the original input to Node_Literal had a datatype or not.
     ;; I guess that means #string needs to match either itself or the
     ;; empty datatype?
     :di (.getLiteralDatatypeURI thing)} 
    {:o (->query-val thing)}))

(defn spo->query
  [s p o]
  (into {} (filter second (merge
                           {:s (->query-val s)
                            :p (->query-val p)}
                           (->query-obj o)))))

(def g
  (proxy [GraphBase] []
    (graphBaseFind
      ([s p o]
       (println "SPO->QUERY :: " (str (spo->query s p o)))
       (wrapped-iterator
        (map (fn [m]
               (println "  CONVERTING" (str m))
               (map->triple m))
             (query-stream
              (spo->query s p o)
              (:maps @st/state))))))))

(def m (ModelFactory/createModelForGraph g))
(def q (QueryFactory/create "select * where {?s <http://example.com/p> ?o ; ?p \"1\"}"))

(->> (QueryExecutionFactory/create q m)
     (.execSelect)
     iterator-seq
     first)
