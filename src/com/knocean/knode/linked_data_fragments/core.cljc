(ns com.knocean.knode.linked-data-fragments.core
  (:require [clojure.edn :as edn]
            [clojure.string :as string]
            [clojure.set :as set]
            [honeysql.core :as sql]

            [org.knotation.util :as util]
            [org.knotation.rdf :as rdf]

            [com.knocean.knode.linked-data-fragments.base :as base :refer [query query-stream]]

            [com.knocean.knode.state :as st]))

(defn nquads-object->object
  [object]
  (throw (Exception. "TODO: object->nquads-object")))

;;;;; Query parsing/manipulating
(defn string->object
  [s]
  (try
    (let [res (nquads-object->object s)]
      (base/remove-falsies
       [[:oi (::rdf/iri res)]
        [:ol (::rdf/lexical res)]
        [:lt (::rdf/language res)]
        [:di (::rdf/datatype res)]]))
    (catch Exception e
      {:oi s})))

(defn req->query
  [req]
  (merge
   {:per-page base/+per-page+ :page 0}
   (if-let [obj (get-in req [:params "object"])]
     (string->object obj))
   (base/updates
    (set/rename-keys
     (clojure.walk/keywordize-keys
      (select-keys
       (:params req)
       ["graph" "subject" "predicate" "per-page" "page"]))
     {:graph :gi :subject :si :predicate :pi})
    :per-page #(min base/+per-page+ (max 1 (base/str->int % base/+per-page+)))
    :page #(max 0 (base/str->int % 0)))))

(defn query->sql
  [{:keys [per-page page] :as query}]
  (let [wheres (map (fn [[k v]] [:= k v]) (select-keys query st/columns))]
    (sql/build
     :select :* :from :states
     :where (when (not (empty? wheres)) (vec (cons :and wheres)))
     :limit per-page :offset (when (> page 0) (* page per-page)))))

;;;;; Query handling
(defn matches-query?
  [query entry]
  (let [blanks {:si :sb :oi :ob}]
    (every?
     identity
     (map (fn [k]
            (let [q (get query k)]
              (or (nil? q)
                  (= q (get entry k))
                  (and (= ":blank" q) (get entry (get blanks k))))))
          [:gi :si :pi :oi :ol :lt :di]))))

(defn paginated [per-page pg seq]
  {:total (count seq)
   :per-page per-page
   :page pg
   :items (vec (take per-page (drop (* per-page pg) seq)))})

(defn query!
  [query-map]
  (cond
    (:connection @st/state)
    (let [q (query->sql query-map)
          total (first (vals (first (st/query (assoc q :select [:%count.*])))))]
      {:total total :per-page (:per-page query-map) :page (:page query-map)
       :items (vec (map #(into {} (filter second %)) (st/query q)))})
    
    (:maps @st/state)
    (paginated (:per-page query-map) (:page query-map)
               (filter (partial matches-query? query-map) (:maps @st/state)))
    
    :else nil))
