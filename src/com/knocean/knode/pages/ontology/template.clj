(ns com.knocean.knode.pages.ontology.template
  (:require [clojure.string :as string]
            [clojure.set :as set]

            [org.knotation.link :as ln]
            [org.knotation.rdf :as rdf]
            [org.knotation.rdfa :as rdfa]
            [org.knotation.environment :as en]
            [org.knotation.state :as s]

            [com.knocean.knode.state :refer [state] :as st]
            [com.knocean.knode.pages.html :refer [html]]
            [com.knocean.knode.pages.ontology.base :refer [ontology-result] :as base]))

(def template-content "https://knotation.org/predicate/template-content")

(defn list-templates []
  (->> (:states @st/state)
       (filter #(= {::rdf/iri template-content} (::rdf/predicate %)))
       (map (comp ::rdf/iri ::rdf/subject))))

(defn template-by-iri [iri]
  (->> (:states @st/state)
       (filter #(and (= {::rdf/iri iri} (::rdf/subject %))
                     (= ::s/statement (::s/event %))))
       (map (fn [s]
              (let [input (::s/input s)
                    content (string/join \newline (::s/lines input))
                    preds (set (map second (re-seq #"\{(.*?)\}" content)))
                    src {:source (::s/source input)
                         :line-number (::s/line-number input)
                         :format (::s/format input)
                         :lines (vec (::s/lines input))}
                    env (st/latest-env)]
                {:template content
                 :predicates preds
                 :input src
                 :name (ln/iri->name env iri) :iri iri})))
       first))

(defn template-dummy
  [template]
  (string/join
   \newline
   (concat
    [": SUBJECT"
     (str "knp:apply-template: " (:name template))]
    (map #(str " " % ": REQUIRED") (:predicates template)))))

(defn parse-content [raw-content]
  (->> raw-content string/split-lines
       (map #(subs % 1))
       (map #(string/split % #": "))
       (into {})))

(defn valid-application?
  [template content]
  (= (:predicates template) (set (keys content))))

(defn validate-application
  ([template content] (validate-application (st/latest-env) template content))
  ([env template content]
   (let [unknowns (->> content
                       (filter #(nil? (ln/subject->iri env (second %))))
                       (into {}))
         base (if (empty? unknowns) {} {:warnings {:unknown-values unknowns}})]
     (if (valid-application? template content)
       base
       (assoc
        base :errors
        {:extra-predicates (set/difference (set (keys content)) (:predicates template))
         :missing-predicates (set/difference (:predicates template) (set (keys content)))})))))
