(ns com.knocean.knode.pages.ontology.core
  (:require [cheshire.core :as json]

            [org.knotation.link :as ln]
            [org.knotation.rdf :as rdf]
            [org.knotation.environment :as en]
            [org.knotation.json-ld :as json-ld]
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

(defn subject-by-iri
  [iri]
  (let [env (st/latest-env)
        relevant (st/select (format "si='%s'" iri))]
    (map (fn [statement]
           (if-let [pi (:pi statement)]
             [(ln/iri->name env pi)
              (ln/iri->name env (:oi statement))]
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
   :body
   (json-ld/render-stanza
    (st/latest-env)
    (first requested-iris)
    (concat
     (st/latest-prefix-states)
     (st/select (format "si='%s'" (first requested-iris)))))})

(defmethod ontology-result "ttl"
  [{:keys [requested-iris params env] :as req}]
  {:status 200
   :body
   (let [iri (first requested-iris)
         resource (or (:resource params)
                      (:project-name @state))
         states (concat
                 (st/latest-prefix-states)
                 (if iri
                   (->> iri
                        (format "si='%s'")
                        st/select
                        rdf/assign-stanzas)
                   (st/select (format "rt='%s'" resource))))]
     (kn/render-string :ttl (st/latest-env) states))})

; TODO: This is pulled out just to make REPL development easier.
(defn inner-ontology-request
  [{:keys [:request-method] :as req}]
  (if (= request-method :put)
    (edit/add-term req)
    ((base/with-requested-terms ontology-result) req)))

(defn ontology-request
  [req]
  (inner-ontology-request req))

(def routes
  [["/ontology/add-term" (auth/logged-in-only (base/with-requested-terms #(edit/add-term %)))]
   ["/ontology/validate-term" (auth/logged-in-only (base/with-requested-terms #(edit/validate-term %)))]
   ["/ontology" [[true ontology-request]]]])
