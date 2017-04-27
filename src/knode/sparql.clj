(ns knode.sparql
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [knode.state :refer [state]]
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
    (let [repo (BigdataSailRepository. (BigdataSail. props))]
      (.initialize repo)
      (swap! state assoc :blazegraph repo))))

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
(def subclass-of "<http://www.w3.org/2000/01/rdf-schema#subClassOf")
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
  [{:keys [env context terms blazegraph] :as state}]
  (let [path "tmp/terms.ttl"]
    (io/make-parents path)
    (spit path (emit/emit-ttl-terms env context terms))
    (with-open [conn (.getConnection blazegraph)]
      (try
        (doto conn
          (.begin)
          (.add (io/file path)
                "base:"
                RDFFormat/TURTLE
                (->> "http://example.com/example"
                     (.createURI (.getValueFactory conn))
                     vector
                     (into-array)))
          (.commit))
        (catch RepositoryException e
          (.rollback conn))))))

(defn get-value
  [v]
  (cond
    (instance? URI v) {:iri (str v)}
    (instance? BNode v) {:bnode (str v)}
    (instance? Literal v)
    (cond
      (.getLanguage v) {:lexical (.getLabel v) :language (.getLanguage v)}
      (.getDatatype v) {:lexical (.getLabel v) :datatype (str (.getDatatype v))}
      :else {:lexical (.getLabel v)})))

(defn select
  [{:keys [blazegraph] :as state} query]
  (with-open [conn (.getConnection blazegraph)]
    (let [tq (.prepareTupleQuery conn QueryLanguage/SPARQL query)
          results (atom [])]
      (.setMaxQueryTime tq 2)
      (with-open [tr (.evaluate tq)]
        (let [vs (.getBindingNames tr)]
          (while (.hasNext tr)
            (let [bindings (.next tr)]
              (swap!
               results
               conj
               (->> vs
                    (map (juxt identity #(get-value (.getValue bindings %))))
                    (into {})))))))
      @results)))

(defn validate
  [{:keys [validation-rules] :as state}]
  (->> validation-rules
       (map
        (fn [rule]
          (let [results (select state (:select rule))]
            (if (first results)
              (assoc rule :results results)
              rule))))
       (filter :results)
       doall))
