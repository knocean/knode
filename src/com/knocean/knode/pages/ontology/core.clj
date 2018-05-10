(ns com.knocean.knode.pages.ontology.core
  (:require [cheshire.core :as json]

            [org.knotation.link :as ln]
            [org.knotation.rdf :as rdf]
            [org.knotation.clj-api :as kn]

            [com.knocean.knode.state :refer [state] :as st]
            [com.knocean.knode.pages.mimetypes :as mime]
            [com.knocean.knode.pages.html :refer [html]]
            [com.knocean.knode.pages.term-status :as stat]

            [com.knocean.knode.pages.ontology.base :as base :refer [ontology-result]]
            [com.knocean.knode.pages.authentication :as auth]
            [com.knocean.knode.pages.ontology.html]
            [com.knocean.knode.pages.ontology.tsv]
            [com.knocean.knode.pages.ontology.edit :as edit]))

; TODO: What does this do?
(defn with-name [env iri]
  (ln/iri->name env iri))

(defn subject-by-iri
  [iri]
  (let [env (st/latest-env)
        relevant (->> @state :states
                      (filter #(= (:si %) iri))
                      (filter #(not= :org.knotation.state/subject-end (:org.knotation.state/event %))))]
    (map (fn [statement]
           (if-let [pi (:pi statement)]
             [(with-name env pi)
              (with-name env (:oi statement))]
             [:subject (:si statement)]))
         relevant)))

(defmethod ontology-result :default
  [{:keys [query-params] :as req}]
  (html
   {:status 406
    :title "Ontology Page"
    :error [:span "Unsupported content type" (mime/req->content-type req) ". Please request one of"
            [:ul (map (fn [[mimetype format]]
                        [:li (mime/req->format-link req format)])
                      mime/mimetype-table)]]}))

(defmethod ontology-result "json"
  [{:keys [requested-iris env] :as req}]
  {:status 200
   :body (json/encode
          (if (first requested-iris)
            (subject-by-iri (first requested-iris))
            (base/all-subjects)))})

(defmethod ontology-result "ttl"
  [{:keys [requested-iris env] :as req}]
  {:status 200
   :body
   (let [iri (first requested-iris)
         _ (println "IRI" iri)
         states (if iri
                  (->> @state
                       :states
                       (filter (fn [{:keys [si]}] (if si (= iri si) true))))
                  (:states @state))]
     (kn/render-string :ttl (st/latest-env) states))})

(def routes
  [["/ontology/add-term" (auth/logged-in-only (base/with-requested-terms #(edit/add-term %)))]
   ["/ontology/validate-term" (auth/logged-in-only (base/with-requested-terms #(edit/validate-term %)))]
   ["/ontology" [[true (base/with-requested-terms ontology-result)]]]])
