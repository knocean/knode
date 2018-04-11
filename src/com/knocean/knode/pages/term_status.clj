(ns com.knocean.knode.pages.term-status
  (:require
   [clojure.string :as string]
   [cheshire.core :as json]

   [org.knotation.link :as ln]
   [org.knotation.rdf :as rdf]
   [org.knotation.rdfa :as rdfa]
   [org.knotation.environment :as en]

   [com.knocean.knode.pages.mimetypes :as mime]
   [com.knocean.knode.state :refer [state] :as st]
   [com.knocean.knode.pages.html :refer [html]]))

(def deprecated "http://www.w3.org/2002/07/owl#deprecated")
(def replacement "http://purl.obolibrary.org/obo/IAO_0100001")
(def alternative "http://purl.obolibrary.org/obo/IAO_0000118")

(defn term-status
  [iri]
  (let [relevant (->> @state :states
                      (filter #(= (get-in % [::rdf/subject ::rdf/iri]) iri)))
        recognized (not (empty? relevant))
        obsolete (not
                  (empty?
                   (filter
                    #(and (= deprecated (::rdf/iri (::rdf/predicate %)))
                          (= "true" (::rdf/lexical (::rdf/object %))))
                    relevant)))]
    (reduce
     (fn [memo state]
       (let [predicate (::rdf/predicate state)]
         (if-let [iri (::rdf/iri predicate)]
           (update memo iri #(let [obj (::rdf/object state)]
                               (if % (conj % obj) [obj])))
           memo)))
     {:IRI iri :recognized recognized :obsolete (or (and recognized obsolete) "")}
     relevant)))

(defmulti term-status-result mime/req->output-format)
(defmethod term-status-result :default
  [req]
  (html
   {:status 406
    :session (:session req)
    :title "Term Status Page"
    :error [:span "Unsupported content type" (mime/req->content-type req) ". Please request one of"
            [:ul (map (fn [[mimetype format]]
                        [:li (mime/req->format-link req format)])
                      mime/mimetype-table)]]}))

(defmethod term-status-result "html"
  [{:keys [query-params session] :as req}]
  (let [iri (get query-params "iri")
        stat (term-status iri)]
    (html
     {:status 406
      :session session
      :title "Term Status Page"
      :error [:table.table
              [:tr (map (fn [[k _]] [:th (str (name k))]) stat)]
              [:tr (map (fn [[_ v]] [:td (str v)]) stat)]]})))

(defmethod term-status-result "json"
  [{:keys [query-params] :as req}]
  {:status 200
   :headers {"Content-Type" "application/json; charset=utf-8"}
   :body (json/encode (term-status (get query-params "iri")))})

(def routes
  [["/term-status" (mime/with-content-header term-status-result)]])
