(ns com.knocean.knode.pages.ontology.base
  (:require [clojure.string :as string]

            [com.knocean.knode.pages.mimetypes :as mime]

            [com.knocean.knode.state :refer [state] :as st]
            [org.knotation.link :as ln]))

(defmulti ontology-result mime/req->output-format)

(defn all-subjects
  []
  (->> @state :states
       (filter #(= :org.knotation.state/subject-start (:org.knotation.state/event %)))
       (map :org.knotation.rdf/subject)))

(defn parse-body-terms
  [req]
  (when (and (= :post (:request-method req))
             (= "GET" (get-in req [:params "method"])))
    (rest (string/split-lines (slurp (:body req))))))

(defn parse-request-terms
  [env {:keys [params] :as req}]
  (let [remaining-path (vec (drop 2 (string/split (:uri req) #"/")))
        path-var (when (last remaining-path) (string/replace (last remaining-path) #"\.....?$" ""))
        main-iri (or (get params "iri")
                     (and path-var (ln/subject->iri env path-var))
                     path-var)
        ->terms (fn [s]
                  (when s
                    (if-let [_operator (second (re-find #"^(in|eq)\." s))]
                      (string/split (subs s 3) #" +"))))]
    (map
     #(ln/subject->iri env %)
     (concat
      (when main-iri [main-iri])
      (->terms (get params "CURIE"))
      (->terms (get params "IRI"))
      (parse-body-terms req)))))

(defn with-requested-terms
  [f]
  (mime/with-content-header
    (fn [{:keys [params] :as req}]
      (let [env (st/latest-env)
            remaining-path (vec (drop 2 (string/split (:uri req) #"/")))]
        (f (assoc
            req
            :env env
            :remaining-path remaining-path
            :requested-iris (vec (parse-request-terms env req))))))))
