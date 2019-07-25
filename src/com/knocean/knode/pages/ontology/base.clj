(ns com.knocean.knode.pages.ontology.base
  (:require [clojure.string :as string]

            [com.knocean.knode.pages.mimetypes :as mime]

            [com.knocean.knode.state :refer [state] :as st]
            [com.knocean.knode.util :as util]
            
            [org.knotation.environment :as en]))

(defmulti ontology-result mime/req->output-format)

(defn all-subjects
  []
  (->> {:select [:si] :modifiers [:distinct]
        :from [:states] :where [:= :rt (:project-name @state)] :order-by [:si]}
       st/query
       (map :si)
       (remove nil?)
       (filter #(.startsWith % (:base-iri @state)))))

(defn parse-body-terms
  [req]
  (when (and (= :post (:request-method req))
             (= "GET" (get-in req [:params "method"]))
             (:body-string req))
    (rest (string/split-lines (:body-string req)))))

(defn parse-request-terms
  [env {:keys [params uri] :as req}]
  (let [->terms (fn [s]
                  (when s
                    (if-let [_operator (second (re-find #"^(in|eq)\." s))]
                      (string/split (subs s 3) #" +"))))]
    (map
     #(util/->iri env %)
     (concat
      (when (.startsWith uri (str "/ontology/" (:project-name @state) "_"))
        [(-> uri
             (string/replace #"^/ontology/" "")
             (string/replace #"\.ttl$" "")
             (string/replace #"\.json$" "")
             (string/replace #"\.tsv$" "")
             (string/replace "_" ":"))])
      (->terms (get params "CURIE"))
      (->terms (get params "IRI"))
      (parse-body-terms req)))))

(defn with-requested-terms
  [f]
  (mime/with-content-header
    (fn [{:keys [params uri requested-iris] :as req}]
      (let [env (st/latest-env)
            remaining-path (vec (drop 2 (string/split (:uri req) #"/")))
            req (assoc req :body-string (when (:body req) (-> req :body slurp)))]
        (f (assoc
            req
            :env env
            :remaining-path remaining-path
            :requested-iris (or requested-iris (vec (parse-request-terms env req)))))))))
