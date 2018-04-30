(ns com.knocean.knode.jena
  (:import (org.apache.jena.riot RDFDataMgr)
           (org.apache.jena.rdf.model ModelFactory)
           (org.apache.jena.query QueryFactory QueryExecutionFactory)
           (org.apache.jena.graph NodeFactory Triple)
           (org.apache.jena.graph.impl GraphBase)))

(def g (RDFDataMgr/loadGraph "junk/root.owl"))
(def m (ModelFactory/createModelForGraph g))
(def q (QueryFactory/create "select * where {?s <http://example.com/p> ?o ; ?p \"1\"}"))
;; (->> (QueryExecutionFactory/create q m)
;;      (.execSelect)
;;      iterator-seq
;;      first)

(defn make-triple
  []
  (new Triple
       (NodeFactory/createURI "http://example.com/s")
       (NodeFactory/createURI "http://example.com/p")
       (NodeFactory/createLiteral "1")))

(defn seq->iterator
  [xs]
  (let [xs (atom xs)]
    (reify
      java.util.Iterator
      (hasNext [this] (not= 0 (count @xs)))
      (next [this] (let [x (first @xs)] (reset! xs (rest @xs)) x))
      (remove [this]))))

(def g
  (proxy [GraphBase] []
    (graphBaseFind
      ([s p o]
       (println "THREE" s p o)
       (org.apache.jena.util.iterator.WrappedIterator/create
        (seq->iterator
         [(make-triple)]))))))
