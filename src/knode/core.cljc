(ns knode.core
  (:require [knode.util :as util]))

(def basic-lines
  "@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#>
@prefix xsd: <http://www.w3.org/2001/XMLSchema#>
@prefix owl: <http://www.w3.org/2002/07/owl#>
@prefix obo: <http://purl.obolibrary.org/obo/>
@prefix oio: <http://www.geneontology.org/formats/oboInOwl#>
@prefix IAO: <http://purl.obolibrary.org/obo/IAO_>
@prefix NCBITaxon: <http://purl.obolibrary.org/obo/NCBITaxon_>
@prefix ONTIE: <https://ontology.iedb.org/ONTIE_>

: Mus musculus BALB/c
type: owl:Class
alternative term: BALB/c
subclass of: Mus musculus
rank: subspecies
obsolete: false> boolean")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; Parsing pass
(defn string->iriref [string]
  (let [[_ iri] (re-find #"^<([^\u0000-\u0020<>\"{}|^`\\\\]*)>" string)]
    (when iri {:type :iri :iriref iri})))

(defn string->prefixed-name [string]
  (let [[_ prefix name] (re-find #"^(\w+):([^\s:/][^\s:\\]*)$" string)]
    (when (and prefix name)
      {:type :prefixed-name :prefix prefix :name name})))

(defn string->name [string]
  (or (string->iriref string)
      (string->prefixed-name string)
      {:type :string :string string})) ;; We can't expand labels until we get to the environment pass

(defn string->name-or-blank [string]
  (if-let [blank (re-find #"^_:[-0-9\u00B7\u0300-\u036F\u203F-\u2040A-Za-z\u00C0-\u00D6\u00D8-\u00F6\u00F8-\u02FF\u0370-\u037D\u037F-\u1FFF\u200C-\u200D\u2070-\u218F\u2C00-\u2FEF\u3001-\uD7FF\uF900-\uFDCF\uFDF0-\uFFFD]+$" string)]
    {:blank blank}
    (string->name string)))

;; TODO - Add some serious error handling here
(defn parse-declaration [ln]
  (let [[_ declaration] (re-find #"^@(\w+)\s+" ln)]
    (assoc
     (case declaration
       "base"   (let [[_ target] (re-find #"^@base\s+(\S+?)\s*$" ln)]
                  {:target (string->iriref target)})
       "prefix" (let [[_ name target] (re-find #"^@prefix\s+(\w+?):\s+(\S+?)\s*$" ln)]
                  {:name name :target (string->iriref target)})
       "label"  (let [[_ name target _ datatype] (re-find #"^@label\s+(.*?):\s+(.*?)( > \s*(.*))?\s*$" ln)]
                  {:target (or (string->iriref target)
                               (string->prefixed-name target))
                   :name name
                   :datatype (when datatype (string->name-or-blank datatype))})
       "graph"  (let [[_ target] (re-find #"^@graph\s+(\S+?)\s*$" ln)]
                  {:target (string->name target)}))
     :type (keyword declaration))))

(defn parse-subject-line [ln]
  (let [[_ name] (re-find #"^:\s+(.*)" ln)]
    {:type :subject :target (string->name-or-blank name)}))

(defn parse-predicate+object [ln]
  (let [[_ predicate _ object _ datatype] (re-find #"^(.*?):\s+((.*?)(> (.*))?)$" ln)]
    {:type :predicate+object
     :predicate (string->name-or-blank predicate)
     :object (assoc (string->name-or-blank object) :datatype datatype)}))

(defn parse-line [ln]
  (assoc
   (cond (re-find #"^\s*#.*$" ln) {:type :comment :comment ln}
         (re-find #"^@" ln)   (parse-declaration ln)
         (re-find #"^: ?" ln) (parse-subject-line ln)
         (or (empty? ln) (re-matches #"\s+" ln)) {:type :blank-line}
         (re-find #": " ln) (parse-predicate+object ln)
         :else {:type :unparsed})
   :origin ln))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; Processing pass

;;;; Propagate information to statement blocks
;; TODO - may actually want to break this up into separate propagators for base, graph and subject. If you keep it lazy and effect-free, there's no additional overhead from additional traversals, and it might simplify things.
(defn propagate-subjectives [parsed-lines]
  ((fn rec [lines graph subject]
     (when (not (empty? lines))
       (lazy-seq
        (let [ln (first lines)
              next (fn [head graph subject]
                     (cons head (rec (rest lines) graph subject)))]
          (case (:type ln)
            :graph   (next ln (:target ln) subject)
            :subject (next ln graph (:target ln))
            :predicate+object (next
                               (assoc ln :type :statement :graph graph :subject subject)
                               graph subject)
            (next ln graph subject))))))
   parsed-lines nil nil))

;;;;; Collect and process environments

;; TODO - throw error if an additional @base is declared
(defn collect-environment
  ([parsed-lines] (collect-environment {} parsed-lines))
  ([starting-env parsed-lines]
   (reduce
    (fn [env ln]
      (if (contains? #{:label :prefix :base} (:type ln))
        (assoc-in
         env
         (case (:type ln)
           :label  [:labels (:name ln)]
           :prefix [:prefixes (:name ln)]
           :base   [:base])
         (:target ln))
        env))
    starting-env
    parsed-lines)))

;;;;; Expand links
;; (at the end 'cause we need complete environments, minus resolution to take this step properly)
(defn expand-prefixed-name [env link-map]
  (let [prefix (get-in env [:prefixes (:prefix link-map)])]
    (assoc link-map :iriref (str prefix (:name link-map)))))

(defn expand-string [env link-map]
  (if (get-in env [:labels (:string link-map)])
    {:type :label-name :name (:string link-map)}
    {:type :literal :string (:string link-map)}))

(defn expand-iri-map [env link-map]
  (update link-map :iriref (partial util/expand-iri (:base env))))

(defn expand-link [env link-map]
  (case (:type link-map)
    :string (expand-string env link-map)
    :prefixed-name (expand-prefixed-name env link-map)
    :iri (expand-iri-map env link-map)))

(defn map-vals [f dict]
  (into {} (map (fn [[k v]] [k (f v)]) dict)))

(defn expand-environment [env]
  (let [expanded-prefixes (update env :prefixes #(map-vals (partial expand-iri-map env) %))]
    (update expanded-prefixes :labels #(map-vals (partial expand-link env) %))))

;; (defn expand-all-links [expanded-env propagated-lines]
;;   )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; External interface
(defn parse-lines [line-seq]
  (let [propagated (propagate-subjectives (map parse-line line-seq))
        env (expand-environment (collect-environment propagated))]
    {:env env :forms propagated}))
