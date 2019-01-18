(ns com.knocean.knode.pages.ontology.core
  (:require [cheshire.core :as json]
            [clojure.string :as string]

            [org.knotation.rdf :as rdf]
            [org.knotation.json-ld :as json-ld]
            [org.knotation.clj-api :as kn]
            [org.knotation.state :as knst]

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
        relevant (st/select [:= :si iri])]
    (map (fn [statement]
           (if-let [pi (:pi statement)]
             [(en/iri->name env pi)
              (en/iri->name env (:oi statement))]
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

(defn assign-rdf
  ""
  [quads]
  (reduce
    (fn [quads quad]
      (conj quads {::knst/quad quad}))
    []
    (map
      #(clojure.set/rename-keys 
        (select-keys % [:di :oi :si :gi :ln :dt :zn :ol])
        {:di ::rdf/di
         :oi ::rdf/oi
         :si ::rdf/si
         :gi ::rdf/gi
         :ln ::rdf/ln
         :dt ::rdf/dt
         :zn ::rdf/zn
         :ol ::rdf/ol})
      quads)))

(defmethod ontology-result "json"
  [{:keys [requested-iris env] :as req}]
  {:status 200
   :body
   (json-ld/render-stanza
    (st/latest-env)
    (first requested-iris)
    (concat
     (st/latest-prefix-states)
     (assign-rdf (st/select [:= :si (first requested-iris)]))))})

(defmethod ontology-result "ttl"
  [{:keys [requested-iris params env] :as req}]
  {:status 200
   :body
   (let [iri (first requested-iris)
         resource (or (:resource params)
                      (:project-name @state))
         states (concat
                 (st/latest-prefix-states)
                 (->> (if iri [:= :si iri] [:= :rt resource])
                      st/select
                      assign-rdf))]
     (kn/render-string :ttl (st/latest-env) states))})

(defn ontology-request
  [{:keys [:request-method] :as req}]
  (if (= request-method :put)
    (edit/add-term req)
    ((base/with-requested-terms ontology-result) req)))

(def routes
  [["/ontology/add-term" (->> edit/add-term auth/api-key-only base/with-requested-terms)]
   ["/ontology/validate-term" (auth/logged-in-only (base/with-requested-terms #(edit/validate-term %)))]
   ["/ontology" [[true #(ontology-request %)]]]])
