(ns knode.text-search
  (:require [clucy.core :as clucy]
            [me.raynes.fs :as fs]

            [knode.state :refer [state]]))

(def +max-search-results+ 100)
(def +search-index-dir+ "tmp/search-index")
(def index (clucy/disk-index +search-index-dir+))

(def rdf-label "http://www.w3.org/2000/01/rdf-schema#label")
(def alternative-term "http://purl.obolibrary.org/obo/IAO_0000118")
(def indexed-predicates
  #{rdf-label alternative-term})

(defn clear-index! []
  (fs/delete-dir +search-index-dir+))

(defn add! [map] (clucy/add index map))

(defn add-term! [term]
  (let [[iri subject-map] term
        index-map (merge
                   {:iri iri}
                   (->> subject-map
                        :blocks
                        (filter #(contains? indexed-predicates (get-in % [:predicate :iri])))
                        (map #(let [o (:object %)]
                                [(keyword (get-in % [:predicate :label]))
                                 (or (:lexical o) (:name o) (:label o) (:iri o))]))
                        (into {})))]
    (add! index-map)))

(defn populate-index! []
  (doseq [term (:terms @state)]
    (add-term! term)))

(defn search [term]
  (map
   clojure.walk/stringify-keys
   (try
     (clucy/search index term +max-search-results+)
     (catch org.apache.lucene.store.NoSuchDirectoryException e
       []))))

(defn complete [term]
  [])
