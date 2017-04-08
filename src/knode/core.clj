(ns knode.core)

(def basic-lines
  "@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#>
@prefix xsd: <http://www.w3.org/2001/XMLSchema#>
@prefix owl: <http://www.w3.org/2002/07/owl#>
@prefix obo: <http://purl.obolibrary.org/obo/>
@prefix oio: <http://www.geneontology.org/formats/oboInOwl#>
@prefix IAO: <http://purl.obolibrary.org/obo/IAO_>
@prefix NCBITaxon: <http://purl.obolibrary.org/obo/NCBITaxon_>
@prefix ONTIE: <https://ontology.iedb.org/ONTIE_>> a:prefix

: Mus musculus BALB/c
type: owl:Class
alternative term: BALB/c
subclass of: Mus musculus
rank: subspecies
obsolete: false> boolean")

(defn string->iriref [string]
  (let [[_ iri] (re-find #"^<([^\u0000-\u0020<>\"{}|^`\\\\]*)>" string)]
    (when iri {:iriref iri})))

(defn string->prefixed-name [string]
  (let [[_ prefix name] (re-find #"^(\w+):([^\s:/][^\s:\\]*)$" string)]
    (when (and prefix name)
      {:prefix prefix :name name})))

(defn string->name [string]
  (or (string->iriref string)
      (string->prefixed-name string)
      {:string string}))

(defn string->name-or-blank [string]
  (if-let [blank (re-find #"^_:[-0-9\u00B7\u0300-\u036F\u203F-\u2040A-Za-z\u00C0-\u00D6\u00D8-\u00F6\u00F8-\u02FF\u0370-\u037D\u037F-\u1FFF\u200C-\u200D\u2070-\u218F\u2C00-\u2FEF\u3001-\uD7FF\uF900-\uFDCF\uFDF0-\uFFFD]+$" string)]
    {:blank blank}
    (string->name string)))

(defn parse-environment-directive [ln]
  (let [[_ keyword name target _ datatype] (re-find #"^@(\w+)\s+(.*?): \s*(.*?)(> \s*(.*))?$" ln)]
    (assoc
     (case keyword
       "prefix" {:type :prefix}
       "label" {:type :label}
       "base" {:type :base}
       "graph" {:type :graph})
     :name name
     :target (string->name-or-blank target)
     :datatype (when datatype (string->name-or-blank datatype)))))

(defn parse-subject-line [ln]
  (let [[_ name] (re-find #"^:\s+(.*)" ln)]
    {:type :subject :subject (string->name-or-blank name)}))

(defn parse-predicate+object [ln]
  (let [[_ predicate _ object _ datatype] (re-find #"^(.*?):\s+((.*?)(> (.*))?)$" ln)]
    {:type :predicate+object
     :predicate (string->name-or-blank predicate)
     :object (assoc (string->name-or-blank object) :datatype datatype)}))

(defn parse-line [ln]
  (assoc
   (cond (re-find #"^@" ln) (parse-environment-directive ln)
         (re-find #"^: ?" ln) (parse-subject-line ln)
         (or (empty? ln) (re-matches #"\s+" ln)) {:type :blank-line}
         (re-find #": " ln) (parse-predicate+object ln)
         :else {:type :unparsed})
   :origin ln))
