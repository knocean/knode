(ns com.knocean.knode.pages.ontology.template
  (:require [clojure.string :as string]
            [clojure.set :as set]

            [org.knotation.rdf :as rdf]
            [org.knotation.environment :as en]
            [org.knotation.state :as s]

            [com.knocean.knode.util :as util]
            [com.knocean.knode.state :refer [state] :as st]
            [com.knocean.knode.pages.html :refer [html]]
            [com.knocean.knode.pages.ontology.base :refer [ontology-result] :as base]))

(def template-content "https://knotation.org/predicate/template-content")

(defn list-templates []
  (->> {:select [:*] :from [:states]
        :where [:and
                [:= :pi template-content]
                [:= :rt (:project-name @st/state)]]}
       st/query
       (map :si)))

(defn template-by-iri [iri]
  (let [content (->> {:select [:*] :from [:states]
                      :where [:and
                              [:= :rt (:project-name @st/state)]
                              [:= :si iri]
                              [:= :pi template-content]]}
                     st/query
                     first
                     :ol)
        preds (set (map second (re-seq #"\{(.*?)\}" content)))]
    {:template content
     :predicates preds
     :input {}
     :name (en/iri->name (st/latest-env) iri) :iri iri}))

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
                       (filter #(nil? (util/->iri env (second %))))
                       (map (fn [[k v]] [:unrecognized-name v])))
         base (if (empty? unknowns) {} {:warnings unknowns})]
     (if (valid-application? template content)
       base
       (assoc
        base :errors
        (concat
         (map
          (fn [p] [:extra-predicate p])
          (set/difference (set (keys content)) (:predicates template)))
         (map
          (fn [p] [:missing-predicate p])
          (set/difference (:predicates template) (set (keys content))))))))))

(defn parse-template-application
  [env raw-template-input]
  (let [parsed (string/split-lines raw-template-input)
        [_ subject] (string/split (second parsed) #": ")
        template-iri (util/->iri env subject)
        template (template-by-iri template-iri)
        content (string/join \newline (drop 2 parsed))]
    {:iri template-iri
     :template template
     :content content
     :content-map (parse-content content)}))
