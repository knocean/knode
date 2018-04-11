(ns com.knocean.knode.pages.ontology.core
  (:require [cheshire.core :as json]

            [org.knotation.link :as ln]
            [org.knotation.rdf :as rdf]

            [com.knocean.knode.state :refer [state] :as st]
            [com.knocean.knode.pages.mimetypes :as mime]
            [com.knocean.knode.pages.html :refer [html]]
            [com.knocean.knode.pages.term-status :as stat]

            [com.knocean.knode.pages.ontology.base :as base :refer [ontology-result]]
            [com.knocean.knode.pages.authentication :as auth]
            [com.knocean.knode.pages.ontology.html]
            [com.knocean.knode.pages.ontology.tsv]
            [com.knocean.knode.pages.ontology.edit :as edit]))

(defn with-name [env node]
  (assoc node ::rdf/name (ln/node->name env node)))

(defn subject-by-iri
  [iri]
  (let [env (st/latest-env)
        relevant (->> @state :states
                      (filter #(= (get-in % [::rdf/subject ::rdf/iri]) iri))
                      (filter #(not (= :or.knotation.state/subject-end (:org.knotation.state/event %)))))]
    (map (fn [statement]
           (if-let [pred (::rdf/predicate statement)]
             [(with-name env pred)
              (with-name env (::rdf/object statement))]
             [:subject (::rdf/subject statement)]))
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

(def routes
  [["/ontology/add-term" (auth/logged-in-only (base/with-requested-terms #(edit/add-term %)))]
   ["/ontology/validate-term" (auth/logged-in-only (base/with-requested-terms #(edit/validate-term %)))]
   ["/ontology" [[true (base/with-requested-terms ontology-result)]]]])
