(ns knode.emit
  (:require [clojure.string :as string]
            [clojure.data.json :as json]
            [knode.core :as core]))

(defn emit-ttl-statement
  "Given an environment and a block-map with :predicate and :object,
   return a Turtle statement string."
  [env {:keys [predicate object] :as block}]
  (str
   (core/get-curie env (:iri predicate))
   " "
   (cond
     (:language object)
     (format "\"%s\"%s" (:lexical object) (:language object))
     (:datatype object)
     (format "\"%s\"^^%s"
             (:lexical object)
             (core/get-curie env (get-in object [:datatype :iri])))
     (:lexical object)
     (format "\"%s\"" (:lexical object))
     (core/get-curie env (:iri object))
     (core/get-curie env (:iri object))
     :else
     (format "<%s>" (:iri object)))))

(defn emit-ttl-term
  "Given an environment, a sequence of context block-maps (prefixes),
   a subject block-map, and a sequence of block-maps (statements),
   return a stand-alone Turtle string
   with prefixes and a stanza for that subject with those statements."
  [env context-blocks subject blocks]
  (str
   (->> context-blocks
        (filter :prefix)
        (map #(str "@prefix " (:prefix %) ": <" (:iri %) "> ."))
        (string/join "\n"))
   (when context-blocks "\n\n")
   (core/get-curie env (:iri subject))
   "\n  "
   (->> blocks
        (filter :predicate)
        (map (partial emit-ttl-statement env))
        (string/join "\n; "))
   "\n."))

(defn emit-ttl-terms
  "Given an environment, a sequence of context-blocks, and a map from IRI to term map,
   return a Turtle string."
  [env context-blocks terms]
  (string/join
   "\n\n"
   (concat
    [(->> context-blocks
          (filter :prefix)
          (map #(str "@prefix " (:prefix %) ": <" (:iri %) "> ."))
          (string/join "\n"))]
    (->> terms
         (sort-by #(-> % :subject :iri))
         (map
          (fn [{:keys [subject blocks] :as term}]
            (emit-ttl-term env nil subject blocks)))))))

(defn emit-rdfa-statement
  "Given an environment and a block-map with :predicate and :object,
   return a list item using Hiccup for HTML+RDFa."
  [env {:keys [predicate object] :as block}]
  (let [property (core/get-curie env (:iri predicate))]
    [:li
     [:a {:href (:iri predicate)} (core/get-name env (:iri predicate))]
     ": "
     (cond
       (:language object)
       [:span
        {:property property
         :xml:lang (string/replace (:language object) #"^@" "")}
        (:lexical object)]

       (:datatype object)
       [:span
        {:property property
         :datatype (core/get-curie env (get-in object [:datatype :iri]))}
        (:lexical object)]

       (:lexical object)
       [:span {:property property} (:lexical object)]

       :else
       [:a
        {:href (:iri object) :property property}
        (core/get-name env (:iri object))])]))

(defn emit-rdfa-term
  "Given an environment, a sequence of context block-maps (prefixes),
   a subject block-map, and a sequence of block-maps (statements),
   return an HTML+RDFa <ul> element in Hiccup format."
  [env context-blocks subject blocks]
  (apply
   conj
   [:ul
    {:prefix
     (->> context-blocks
          (filter :prefix)
          (map #(str (:prefix %) ": " (:iri %)))
          (string/join "\n"))
     :resource (core/get-curie env (:iri subject))}]
   (->> blocks
        (filter :predicate)
        (map (partial emit-rdfa-statement env))
        vec)))

(defn emit-jsonld-object
  [env {:keys [iri lexical language datatype] :as object}]
  (cond
    iri
    {"@id" (or (core/get-curie env iri) iri)
     "iri" iri
     "label" (core/get-name env iri)}
    (and lexical language)
    {"@value" lexical "@language" (string/replace language "@" "")}
    (and lexical datatype)
    {"@value" lexical "@type" (core/get-curie env (:iri datatype))}
    lexical
    {"@value" lexical}))

(defn emit-jsonld-term
  [env context-blocks subject blocks]
  (reduce
   (fn [coll {:keys [predicate object] :as block}]
     (let [piri (:iri predicate)
           plabel (core/get-name env piri)
           cardinality (get-in env [:labels plabel :cardinality])
           obj (emit-jsonld-object env object)
           objs (if (and (string? cardinality)
                         (re-matches #".*or more$" cardinality))
                  (concat (get-in coll [plabel]) [obj])
                  obj)]
       (-> coll
           (assoc plabel objs)
           (assoc-in ["@context" plabel]
                     {"@id" (or (core/get-curie env piri) piri)
                      "iri" piri}))))
   (let [curie (core/get-curie env (:iri subject))]
     {"@context"
      (merge
       (->> context-blocks
            (filter :prefix)
            (map (juxt :prefix :iri))
            (into {}))
       (->> context-blocks
            (filter :label)
            (map (fn [{:keys [label target] :as block}]
                   (let [iri (:iri target)
                         curie (or (core/get-curie env iri) iri)]
                     [label {"@id" curie "iri" iri}])))
            (into {})))
      "@id" curie
      "iri" (:iri subject)
      "curie" curie})
   (filter :predicate blocks)))

(defn emit-kn-statement
  "Given a block-map, return a Knotation string."
  [env {:keys [prefix iri label target datatype predicate object] :as block}]
  (cond
    ; TODO: comment
    ; TODO: blank
    ; TODO: base
    (and prefix iri)
    (format "@prefix %s: <%s>" prefix iri)
    (and label target datatype)
    (format "@label %s: %s > %s"
            label
            (core/get-curie env (:iri target))
            (or
             (core/get-curie env (:iri datatype))
             (:language datatype)
             (when (keyword? datatype) (name datatype))))
    (and label target)
    (format "@label %s: %s" label (core/get-curie env (:iri target)))
                                        ; TODO: non-default datatypes
    (and predicate (:curie object))
    (format "%s: %s"
            (core/get-name env (:iri predicate))
            (:curie object))
    (and predicate (:lexical object))
    (format "%s: %s"
            (core/get-name env (:iri predicate))
            (:lexical object))
    (and predicate (:iri object))
    (format "%s: %s"
            (core/get-name env (:iri predicate))
            (core/get-name env (:iri object)))))

(defn emit-kn-term
  "Given a sequence of context block-maps (prefixes),
   a subject block-map, and a sequence of block-maps,
   return a Knotation string."
  [env context-blocks subject blocks]
  (string/join
   "\n"
   (remove
    nil?
    (concat
     (map (partial emit-kn-statement env) context-blocks)
     (when context-blocks [""])
     [(str ": " (core/get-curie env (:iri subject)))]
     (map (partial emit-kn-statement env) blocks)))))

(defn emit-kn-terms
  "Given an environment, a sequence of context-blocks, and a map from IRI to term map,
   return a Knotation string."
  [env context-blocks terms]
  ; TODO: context-blocks is currently ignored.
  (->> terms
       (sort-by #(-> % :subject :iri))
       (map
        (fn [{:keys [subject blocks] :as term}]
          (emit-kn-term env nil subject (remove :template blocks))))
       (string/join "\n\n")))

(def rdfs:label "http://www.w3.org/2000/01/rdf-schema#label")
(def rdf:type "http://www.w3.org/1999/02/22-rdf-syntax-ns#type")
(def owl:deprecated "http://www.w3.org/2002/07/owl#deprecated")
(def iao:replacement "http://purl.obolibrary.org/obo/IAO_0100001")

(defn emit-index
  "Given an environment and a map from IRI to term map,
   return a tab-separated index table for the terms."
  [env terms]
  (->> terms
       (sort-by #(-> % :subject :iri))
       (map
        (fn [{:keys [subject blocks] :as term}]
          (let [values (core/collect-values blocks)]
            [(core/get-curie env (:iri subject))
             (get-in values [rdfs:label 0 :lexical])
             (core/get-curie env (get-in values [rdf:type 0 :iri]))
             (get-in values [owl:deprecated 0 :lexical] "false")
             (get-in values [iao:replacement 0 :iri])])))
       (concat [["CURIE" "label" "type" "obsolete" "replacement"]])
       (map (partial string/join "\t"))
       (string/join "\n")))
