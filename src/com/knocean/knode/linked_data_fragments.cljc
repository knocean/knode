(ns com.knocean.knode.linked-data-fragments
  (:require [clojure.edn :as edn]
            [clojure.string :as string]
            [clojure.set :as set]
            [clojure.java.jdbc :as sql]

            [org.knotation.util :as util]
            [org.knotation.rdf :as rdf]
            [org.knotation.object :as ob]))

(def +per-page+ 100)

(defn tap!
  ([v] (tap! "TAPPED" v))
  ([label v]
   (println label (str v))
   v))

(defn remove-falsies
  [sequence]
  (into {} (filter second sequence)))

(defn updates
  [m & {:as k-f-map}]
  (reduce
   (fn [memo [k f]]
     (update memo k f))
   m k-f-map))

(defn str->int
  ([str] (str->int str 0))
  ([str default]
   (util/handler-case
    (Integer/parseInt str)
    (:default e default))))

;;;;; Query parsing
(defn string->object
  [s]
  (util/handler-case
   (let [res (ob/nquads-object->object s)]
     (remove-falsies
      [[:iri (::rdf/iri res)]
       [:label (::rdf/lexical res)]
       [:language (::rdf/language res)]
       [:datatype (::rdf/datatype res)]]))
   (:default e {:iri s})))

(defn req->query
  [req]
  (merge
   {:per-page +per-page+ :pg 0}
   (updates
    (clojure.walk/keywordize-keys
     (select-keys
      (:params req)
      ["graph" "subject" "predicate" "object" "per-page" "pg"]))
    :object #(when % (string->object %))
    :per-page #(min +per-page+ (max 1 (str->int % +per-page+)))
    :pg #(max 0 (str->int % 0)))))

;;;;; Query handling
(defn ?= [a b] (or (nil? a) (= a b)))

(defn matches-query?
  [{:keys [graph subject predicate object] :as query} entry]
  (and (?= graph (:gi entry))
       (?= subject (:si entry))
       (?= predicate (:pi entry))
       (or (nil? object)
           (and (:iri object) (= (:iri object) (:oi entry)))
           (and (= (:label object) (:ol entry))
                (?= (:language object) (:ln entry))
                (?= (:datatype object) (:di entry))))))

(defn paginated [per-page pg seq]
  {:total (count seq)
   :per-page per-page
   :page pg
   :items (vec (take per-page (drop (* per-page pg) seq)))})

(defmulti query
  #(cond
     (or (vector? %2) (list? %2)) :default

     (and (map? %2)
          (set/subset?
           (set (keys %2))
           #{:classname :subprotocol :subname :connection}))
     :database

     :else :default))

(defmethod query :default ;; default is an in-memory sequence, which we just filter.
  [query data]
  (paginated (get query :per-page +per-page+) (get query :pg 0)
             (filter (partial matches-query? query) data)))

(defn -query->where-clause
  [query]
  (let [obj (:object query)
        slots [[query :graph :gi]
               [query :subject :si]
               [query :predicate :pi]
               [obj :iri :oi]
               [obj :label :ol]
               [obj :language :ln]
               [obj :datatype :di]]
        no-nil (fn [seq] (remove nil? seq))
        where-seq (map (fn [[o k label]] (when (get o k) (str (name label) "=?"))) slots)]
    (when (not (empty? where-seq))
      (no-nil
       (cons
        (str " " (string/join " AND " (no-nil where-seq)))
        (map (fn [[o k _]] (get o k)) slots))))))

(defn -query->sql-pagination [{:keys [per-page pg]}]
  (str " " (string/join
            " "
            (remove
             nil? [(str "LIMIT=" per-page)
                   (when (> pg 0) (str "OFFSET=" (* pg per-page)))]))))

(defn query->sql
  [query]
  (let [base "SELECT * FROM ontology"
        [where & params] (-query->where-clause query)
        pagination (-query->sql-pagination query)]
    (vec
     (cons
      (string/join [base where pagination])
      params))))

(defmethod query :database ;; if we've got a database, we need to query it for matching entries
  [query data]
  (sql/with-db-connection [db data]
    (sql/query db (query->sql query))))
