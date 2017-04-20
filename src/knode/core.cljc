(ns knode.core
  (:require [clojure.test :refer [deftest testing is]]
            clojure.set
            [knode.util :as util]))

;; # Overview
;;
;; - names
;; - objects
;; - blocks

;; # Names
;;
;; We have two kinds of names: primary and secondary.
;; A primary name gives us all the information we need.
;; A secondary name has to be resolved to a primary name using the environment.
;; The primary names are:
;;
;; - absolute IRI
;; - blank node
;;
;; Secondary names are resolved to IRIs using part of the environment:
;;
;; - TODO: relative IRI, resolve with @base
;; - curie, resolve with @prefix
;; - label, resolve with @label

;; All names have to be parsed from strings to name-maps.
;; Secondary names have to be resolved to primary names.
;; Labels are a superset of CURIEs,
;; so we first check the environment for a label,
;; and if that fails we try to handle the name as a CURIE.

;; ## Primary Names

(defn primary-name?
  "Given a value, return true only if it is a primary name-map."
  [name-map]
  (boolean
   (and (map? name-map)
        (some #{:iri :bnode} (keys name-map)))))

;; The most important kind of name is an absolute IRI.
;; IRIs are globally unique names.

(defn parse-iri
  "Given a string, try to parse it as an absolute IRI in angle brackets
   and return a name-map."
  [string]
  (let [[_ iri] (re-find #"^<([^\u0000-\u0020<>\"{}|^`\\\\]*)>" string)]
    ; TODO: check that this is absolute.
    (when iri {:iri iri})))

;; We also have blank nodes (bnode),
;; which are unique names local to a document or processing context.
;; Blank nodes are often randomized during processing.

(defn parse-bnode
  "Given a string, try to parse it as a blank node,
   and return a name-map."
  [string]
  (when (re-find #"^_:[-0-9\u00B7\u0300-\u036F\u203F-\u2040A-Za-z\u00C0-\u00D6\u00D8-\u00F6\u00F8-\u02FF\u0370-\u037D\u037F-\u1FFF\u200C-\u200D\u2070-\u218F\u2C00-\u2FEF\u3001-\uD7FF\uF900-\uFDCF\uFDF0-\uFFFD]+$" string)
    {:bnode string}))

;; ## Environment
;;
;; An environment has :prefixes and :labels to resolve CURIEs and labels.
;; Declarations are used to update the environment.

{:prefixes
 {"ex" "http://example.com/"}
 :labels
 {"type"
  {:iri ""
   :datatype :link}}}

(defn add-label
  "Add a label to the environment."
  [env label target datatype]
  (-> env
      (assoc-in [:labels label] {:iri (:iri target) :datatype datatype})
      (assoc-in [:iri-labels (:iri target)] label)))

(defn update-environment
  "Given an environment and a block-map,
   add any prefixes or labels,
   and return the updated environment."
  [env {:keys [prefix iri label target datatype subject] :as block-map}]
  (cond
    (and prefix iri)
    (assoc-in env [:prefixes prefix] iri)
    (and label target)
    (add-label env label target datatype)
    subject
    (assoc env :subject subject)
    :else
    env))

;; ## Secondary Names

(defn secondary-name?
  "Given a value, return true only if it is a primary name-map."
  [name-map]
  (boolean
   (and (map? name-map)
        (some #{:label :curie} (keys name-map)))))

;; ### CURIE
;;
;; A CURIE consists of a prefix and a suffix, separated by a colon.
;; We use @prefix declarations to resolve CURIEs to IRIs.

(def match-curie #"^([a-zA-Z0-9]+):([^\s:/][^\s:\\]*)$")

(defn parse-curie
  "Given a string, try to parse it as a CURIE,
   and return a name-map."
  [string]
  (let [[_ prefix suffix] (re-find match-curie string)]
    (when (and prefix suffix)
      {:curie string})))

(defn resolve-curie
  "Given an environment with :prefixes and a name-map with a :curie,
   return a name-map with an :iri."
  [{:keys [prefixes] :as env} {:keys [curie] :as name-map}]
  (let [[_ prefix suffix] (re-find match-curie curie)
        iri (get prefixes prefix)]
    (if iri
      (assoc name-map :iri (str iri suffix))
      (util/throw-exception
       "Could not find prefix:"
       prefix))))

(defn find-prefix
  [env iri]
  (->> (clojure.set/map-invert (get env :prefixes))
       (sort-by (comp count first) >)
       (filter
        (fn [[prefix-iri prefix]]
          (util/starts-with? iri prefix-iri)))
       first))

(defn get-curie
  [env iri]
  (if-let [[prefix name] (find-prefix env iri)]
    (clojure.string/replace iri prefix (str name ":"))))

;; ### Label
;;
;; A label is a string meant to be meaningful to human readers.
;; It can contain whitespace and special characters.
;; CURIEs are also valid labels.
;; We use @label declarations to resolve labels to IRIs.

(def match-label #"^[^#@:<>\s].*?\S+$")
(def exclude-label #": | > ")

(defn label?
  "Given a value, return true if it is a valid label string."
  [label]
  (and (string? label)
       (re-find match-label label)
       (not (re-find exclude-label label))))

(defn parse-label
  "Given a string, try to parse it as a label,
   and return a name-map."
  [string]
  (when (label? string)
    {:label string}))

(defn resolve-label
  "Given an environment with :labels and a name-map with a :label,
   return a name-map with an :iri."
  [{:keys [labels] :as env} {:keys [label] :as name-map}]
  (let [iri (get-in labels [label :iri])]
    (if iri
      (assoc name-map :iri iri)
      (util/throw-exception
       "Could not resolve label:"
       label))))

;; Now we put this all together:

(defn name?
  "Given a value, return true if it is a name-map."
  [name-map]
  (or (primary-name? name-map)
      (secondary-name? name-map)))

(defn parse-non-label
  "Given a string, try to parse it as a non-label name
   and return a name-map."
  [string]
  (or (parse-iri string)
      (parse-bnode string)
      ; TODO: relative IRI
      (parse-curie string)
      (util/throw-exception
       "Could not parse name:"
       string)))

(defn parse-name
  "Given a string, try to parse it as any type of name."
  [string]
  (or (parse-non-label string)
      (parse-label string)
      (util/throw-exception
       "Could not parse name:"
       string)))

(defn resolve-name
  "Given an environment and a name-map,
   return a name-map with a primary name."
  [env {:keys [iri bnode label curie name] :as name-map}]
  (cond
    iri name-map
    bnode name-map
    label (resolve-label env name-map)
    curie (resolve-curie env name-map)
    (get-in env [:labels name]) (resolve-label env (assoc name-map :label name))
    :else (merge name-map (resolve-name env (parse-non-label name)))))

(defn get-name
  "Given an IRI, return the best name: label, CURIE, or the original IRI."
  [env iri]
  (or
   (get-in env [:iri-labels iri])
   (get-curie env iri)
   ; TODO: relative IRI
   iri))

;; # Objects
;;
;; Objects come in four types:
;;
;; 1. link: just a name, as above
;; 2. plain literal: just a literal string
;; 3. language literal: a literal string and a language tag
;; 4. typed literal: a literal string and an IRI
;;
;; To handle these consistently,
;; we define a :datatype to be one of these four things:
;; :link, :plain, language tag (string), or name-map
;; An object-map contains :content string and a :datatype name-map (or nil).
;; A literal map contains a :lexical string and an optional :language string
;; or :datatype name-map.
;; We use `resolve-content` to turn an object-map into
;; either a name-map or a literal-map.

(def match-language-tag #"^@\w+$")

(defn datatype?
  "Given a value, return true only if it is a valid datatype."
  [datatype]
  (or (= :link datatype)
      (= :plain datatype)
      (and (string? datatype)
           (re-find match-language-tag datatype))
      (name? datatype)))

(defn parse-datatype
  "Given a string, try to parse it as a datatype,
   and return a keyword, string, or name-map."
  [string]
  (cond
    (= "link" string) :link
    (= "plain" string) :plain
    (re-find match-language-tag string) string
    :else (parse-name string)))

(defn resolve-datatype
  "Given an environment and a datatype,
   return a resolved datatype."
  [env {:keys [name] :as datatype}]
  (cond
    (= "link" name) :link
    (= "plain" name) :plain
    (name? datatype) datatype
    (:name datatype) (resolve-name env datatype)
    :else datatype))

(defn resolve-content
  "Given an environment and an object map with :predicate, :datatype, :content,
   return a name-map or a literal-map."
  [env {:keys [predicate datatype content] :as block-map}]
  (if content
    (let [datatype
          (or datatype
              (get-in env [:labels (:label predicate) :datatype])
              :plain)]
      (assoc
       (dissoc block-map :datatype)
       :object
       (cond
         (= :link datatype) (resolve-name env {:name content})
         (= :plain datatype) {:lexical content}
         (string? datatype) {:lexical content :language datatype}
         (name? datatype) {:lexical content :datatype datatype}
         :else
         (util/throw-exception
          "Base datatype:"
          datatype
          block-map))))
    block-map))

;; # Blocks

;; ## Declaration
;;
;; Declarations are used to update the environment.

;; TODO: Base

;; ### Prefix

(defn parse-prefix-line
  "Given a @prefix line, return a block-map"
  [line]
  (let [[_ prefix iri]
        (re-find #"^@prefix\s+(\w+?):\s+<(\S+?)>\s*$" line)]
    {:prefix prefix :iri iri}))

;; ### Label

(defn parse-label-line
  "Given a @label line, return a block-map"
  [line]
  (let [[_ label target _ datatype]
        (re-find #"^@label\s+(.*?):\s+(.*?)( ?> \s*(.*))?\s*$" line)]
    {:label label
     :target {:name target}
     :datatype (when datatype {:name datatype})}))

;; Put it all together.

(defn parse-declaration-line
  "Given a declaration line, return a block-map."
  [line]
  (let [[_ declaration] (re-find #"^@(\w+)\s+" line)]
    (case declaration
      ; TODO: base
      "prefix" (parse-prefix-line line)
      "label" (parse-label-line line)
      (util/throw-exception
       "Unrecognized declaration:"
       line))))

;; ## Subject

(defn parse-subject-line
  "Given a : subject line, return a block map."
  [line]
  (let [[_ subject] (re-find #"^:\s+(.*)" line)]
    {:subject {:name subject}}))

;; ## Statement

(defn parse-statement-line
  "Given a prediact: object statement line, return a block-map."
  [line]
  (let [[_ p+d content] (re-find #"^(.*?):\s+(.*)$" line)
        [_ predicate _ datatype] (re-find #"^(.*?)( > (.*))?$" p+d)]
    {:predicate {:name predicate}
     :datatype (when datatype {:name datatype})
     :content content}))

; Put it all together

(defn parse-line
  "Given a Knotation line, return a block-map."
  [line]
  ;(assoc
  (cond (re-find #"^\s*#.*$" line) {:comment line}
        (re-find #"^@" line)   (parse-declaration-line line)
        (re-find #"^: ?" line) (parse-subject-line line)
        (or (empty? line) (re-matches #"\s+" line)) {:blank line}
        (re-find #": " line) (parse-statement-line line)
        :else {:type :unparsed}))
   ;:origin line

(defn resolve-block
  "Given an environment and a block-map,
   use the environment to resolve everything."
  [env {:keys [target subject predicate datatype] :as block-map}]
  (merge
   block-map
   (when target {:target (resolve-name env target)})
   (when subject {:subject (resolve-name env subject)})
   (when predicate {:predicate (resolve-name env predicate)})
   (when datatype {:datatype (resolve-datatype env datatype)})))

(defn process-line
  "Given an environment and a Knotation line,
   return the updated environment and the processed block-map."
  [env line]
  (let [block
        (->> line
             parse-line
             (resolve-block env)
             (resolve-content env))]
    [(update-environment env block) block]))

(defn process-lines
  "Given an environment and a sequence of lines,
   return the pair of an environment and a sequence of block-maps."
  [env lines]
  (reduce
   (fn [[env blocks] line]
     (let [[env block] (process-line env line)]
       [env (conj blocks block)]))
   [env []]
   lines))
