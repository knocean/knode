(ns knode.sparql
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.pprint :as pprint]
            [knode.state :refer [state]]
            [knode.core :as core]
            [knode.emit :as emit])
  (:import [org.openrdf.model BNode URI Literal]
           [org.openrdf.query QueryLanguage]
           [org.openrdf.rio RDFFormat]
           [org.openrdf.repository RepositoryException]
           [com.bigdata.rdf.store DataLoader]
           [com.bigdata.rdf.sail
            BigdataSail
            BigdataSail$Options
            BigdataSailRepository]))

(defn init-dataset!
  [state]
  (let [props (java.util.Properties.)]
    (with-open [r (io/reader (io/resource "blazegraph.properties"))]
      (.load props r))
    (io/make-parents (.get props "com.bigdata.journal.AbstractJournal.file"))
    ; suppress annoying Blazegraph banner
    (with-open [null (java.io.PrintStream. "/dev/null")]
      (let [out System/out, err System/err]
        (System/setOut null)
        (System/setErr null)
        (try
          (let [repo (BigdataSailRepository. (BigdataSail. props))]
            (.initialize repo)
            (swap! state assoc :blazegraph repo))
          (catch Exception e
            (throw e))
          (finally
            (System/setOut out)
            (System/setErr err)))))))

(defn literal
  [& args]
  (format "\"%s\" " (string/replace (apply str args) "\"" "\\\"")))

(defn ncbi
  [s]
  (format "<http://purl.obolibrary.org/obo/NCBITaxon_%s>"
          (string/replace s #"\s" "_")))

(defn triple
  [s p o]
  (str (string/join " " [s p o]) " .\n"))

(def has-rank "<http://purl.obolibrary.org/obo/ncbitaxon_has_rank>")
(def rdf-type "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>")
(def rdfs-label "<http://www.w3.org/2000/01/rdf-schema#label>")
(def subclass-of "<http://www.w3.org/2000/01/rdf-schema#subClassOf>")
(def owl-class "<http://www.w3.org/2002/07/owl#Class>")
(def owl-deprecated "<http://www.w3.org/2002/07/owl#deprecated>")
(def xsd-true "\"true\"^^<http://www.w3.org/2001/XMLSchema#boolean>")
(def alternative-term "<http://purl.obolibrary.org/obo/IAO_0000118>")
(def term-replaced-by "<http://purl.obolibrary.org/obo/IAO_0100001>")

(defn write-nodes
  [path lines]
  (io/make-parents path)
  (with-open [w (io/writer path)]
    (doseq [line lines]
      (let [[taxid parent rank _] (string/split line #"\t\|\t" 4)
            taxon (ncbi taxid)]
        (.write w (triple taxon rdf-type owl-class))
        (.write w (triple taxon subclass-of (ncbi parent)))
        (.write w (triple taxon has-rank (ncbi rank)))))))

(defn write-names
  [path lines]
  (io/make-parents path)
  (with-open [w (io/writer path)]
    (doseq [line lines]
      (let [line (string/replace line #"\t\|$" "")
            [taxid primary unique name-type] (string/split line #"\t\|\t")
            taxon (ncbi taxid)
            value (if (string/blank? unique) primary unique)
            pred (if (= name-type "scientific name")
                   rdfs-label
                   alternative-term)]
        (.write w (triple taxon pred (literal value)))))))

(defn write-delnodes
  [path lines]
  (io/make-parents path)
  (with-open [w (io/writer path)]
    (doseq [line lines]
      (let [taxid (string/replace line #"\t\|.*" "")
            taxon (ncbi taxid)]
        (.write w (triple taxon rdf-type owl-class))
        (.write w (triple taxon rdfs-label (literal "obsolete taxon " taxid)))
        (.write w (triple taxon owl-deprecated xsd-true))))))

(defn write-merged
  [path lines]
  (io/make-parents path)
  (with-open [w (io/writer path)]
    (doseq [line lines]
      (let [line (string/replace line #"\t\|$" "")
            [taxid replacement] (string/split line #"\t\|\t")
            taxon (ncbi taxid)]
        (.write w (triple taxon rdf-type owl-class))
        (.write w (triple taxon rdfs-label (literal "obsolete taxon " taxid)))
        (.write w (triple taxon owl-deprecated xsd-true))
        (.write w (triple taxon term-replaced-by (ncbi replacement)))))))

(defn load-taxa!
  [{:keys [^BigdataSailRepository blazegraph] :as state} ^String path]
  (println "Writing ntriples...")
  (with-open [r (java.util.zip.ZipFile. path)]
    (with-open [nodes (->> (. r getEntry "nodes.dmp")
                           (. r getInputStream)
                           clojure.java.io/reader)]
      (write-nodes "tmp/ncbi/nodes.nt" (line-seq nodes)))
    (with-open [names (->> (. r getEntry "names.dmp")
                           (. r getInputStream)
                           clojure.java.io/reader)]
      (write-names "tmp/ncbi/names.nt" (line-seq names)))
    (with-open [merged (->> (. r getEntry "merged.dmp")
                            (. r getInputStream)
                            clojure.java.io/reader)]
      (write-merged "tmp/ncbi/merged.nt" (line-seq merged)))
    (with-open [delnodes (->> (. r getEntry "delnodes.dmp")
                              (. r getInputStream)
                              clojure.java.io/reader)]
      (write-delnodes "tmp/ncbi/delnodes.nt" (line-seq delnodes))))
  (println "Loading ntriples...")
  (with-open [conn (.getConnection blazegraph)]
    (.loadFiles
     (DataLoader. (.getTripleStore conn))
     (io/file "tmp/ncbi")
     "base:"
     RDFFormat/NTRIPLES
     "http://purl.obolibrary.org/obo/NCBITaxon"
     (reify java.io.FilenameFilter
       (accept [_ dir name] (.endsWith name ".nt"))))))

(defn load-terms!
  [{:keys [env context terms blazegraph project-iri] :as state}]
  (with-open [conn (.getConnection blazegraph)]
    (let [path "tmp/terms.ttl"
          contexts (->> project-iri
                        (.createURI (.getValueFactory conn))
                        vector
                        (into-array))]
      (io/make-parents path)
      (spit path (emit/emit-ttl-terms env context terms))
      (try
        (doto conn
          (.begin)
          (.remove nil nil nil contexts)
          (.add (io/file path) "base:" RDFFormat/TURTLE contexts)
          (.commit))
        (catch RepositoryException e
          (println (.getMessage e))
          (.rollback conn))))))

(defn get-value-map
  [v]
  (cond
    (instance? URI v) {:iri (str v)}
    (instance? BNode v) {:bnode (str v)}
    (instance? Literal v)
    (cond
      (.getLanguage v) {:lexical (.getLabel v) :language (.getLanguage v)}
      (.getDatatype v) {:lexical (.getLabel v) :datatype (str (.getDatatype v))}
      :else {:lexical (.getLabel v)})))

(defn get-value-string
  [env v]
  (cond
    (instance? URI v) (core/get-curie env (str v))
    (instance? Literal v) (.getLabel v)
    :else (str v)))

(defn select
  [{:keys [blazegraph] :as state} query]
  (with-open [conn (.getConnection blazegraph)]
    (let [tq (.prepareTupleQuery conn QueryLanguage/SPARQL query)
          results (atom [])]
      (.setMaxQueryTime tq 10)
      (with-open [tr (.evaluate tq)]
        (let [variables (.getBindingNames tr)]
          (while (.hasNext tr)
            (let [bindings (.next tr)
                  values (map #(.getValue bindings %) variables)
                  strings (map (partial get-value-string (:env state))
                               values)]
              (->> values
                   (map get-value-map)
                   (zipmap variables)
                   (into {:values strings})
                   (swap! results conj))))))
      @results)))

(def term-status-query "PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX owl: <http://www.w3.org/2002/07/owl#>
PREFIX obo: <http://purl.obolibrary.org/obo/>

SELECT ?subject ?obsolete ?replacement
WHERE {
  VALUES ?subject { <%s> }
  ?subject rdfs:label ?label .
  OPTIONAL { ?subject owl:deprecated ?obsolete . }
  OPTIONAL { ?subject obo:IAO_0100001 ?replacement . }
}")

(defn term-status
  [state iri]
  (let [result (when iri (first (select state (format term-status-query iri))))]
    {:iri iri
     :recognized (not (nil? result))
     :obsolete (if (= "true" (get-in result ["obsolete" :lexical])) true false)
     :replacement (get-in result ["replacement" :iri])}))

(def full-term-query "PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX owl: <http://www.w3.org/2002/07/owl#>
PREFIX obo: <http://purl.obolibrary.org/obo/>

SELECT ~{?~a~^ ~}
WHERE {
  VALUES ?subject { <~a> }
  ?subject rdfs:label ?label .
~{  OPTIONAL { ?subject <~a> ?label . }~^
~}
  OPTIONAL { ?subject owl:deprecated ?obsolete . }
  OPTIONAL { ?subject obo:IAO_0100001 ?replacement . }
}")

(def q "PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX owl: <http://www.w3.org/2002/07/owl#>
PREFIX obo: <http://purl.obolibrary.org/obo/>

SELECT ?subject ?obsolete ?replacement
WHERE {
  VALUES ?obsolete { true }
  ?subject rdfs:label ?label .
~{  OPTIONAL { ?subject <~a> ?label . }~^
~}
  OPTIONAL { ?subject owl:deprecated ?obsolete . }
  OPTIONAL { ?subject obo:IAO_0100001 ?replacement . }
}")

(defn results->sets [results]
  (when (not (every? empty? results))
    (cons (set (map first results))
          (lazy-seq (results->sets (map rest results))))))

(defn full-term
  [state iri predicate-labels]
  (let [query (pprint/cl-format
                  nil full-term-query
                  predicate-labels iri
                  (map #(:iri (core/resolve-name (:env state) {:label %}))
                       predicate-labels))
        result (when (and iri (not (empty? predicate-labels)))
                 (select state query))]
    (into {} (map #(vec (list (keyword (string/lower-case %1)) %2)) predicate-labels (results->sets (map :values result))))))

(defn validate-rule
  [state rule limit]
  (try
    (let [query (str (:select rule) (when limit (str "\nLIMIT " limit)))
          results (select state query)]
      (if (first results)
        (assoc rule :results results)
        rule))
    (catch Exception e
      (let [m (.getMessage e)]
        (assoc rule
               :results [{:error m :values [m]}]
               :error m)))))

(defn validation-failure
  [{:keys [validation-rules] :as state}]
  (->> (for [rule validation-rules]
         (validate-rule state rule 1))
       (filter :results)
       first))

(defn validate-each
  [{:keys [validation-rules] :as state} limit]
  (doall
   (for [rule validation-rules]
     (validate-rule state rule limit))))

(defn validate
  [state]
  (filter :results (validate-each state nil)))
