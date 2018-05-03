(ns com.knocean.knode.linked-data-fragments.core
  (:require [clojure.edn :as edn]
            [clojure.string :as string]
            [clojure.set :as set]
            [clojure.java.jdbc :as sql]

            [org.knotation.util :as util]
            [org.knotation.rdf :as rdf]
            [org.knotation.object :as ob]))

;;; DUMMY DATA
(defn slurps
  [resource]
  (let [s (java.io.PushbackReader. (clojure.java.io/reader (clojure.java.io/resource resource)))]
    (loop [ln (clojure.edn/read {:eof nil} s)
           lines []]
      (if (not (nil? ln))
        (recur (clojure.edn/read {:eof nil} s) (conj lines ln))
        lines))))

(def dat (slurps "obi_core.edn"))
(def db
  {:classname "org.sqlite.JDBC"
   :subprotocol "sqlite"
   :subname "resources/obi_core.db"})
(defn dummy-db!
  [db]
  (sql/with-db-connection [handle db]
    (sql/execute! handle ["drop table if exists ontology"])
    (sql/execute! handle [(sql/create-table-ddl
                           :ontology
                           (map (fn [name] [name :string])
                                [:gi :si :sb :pi :oi :ob :ol :di :ln]))])
    (doseq [d dat] (sql/insert! handle :ontology d))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def +per-page+ 1000)

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
      [[:oi (::rdf/iri res)]
       [:ol (::rdf/lexical res)]
       [:ln (::rdf/language res)]
       [:di (::rdf/datatype res)]]))
   (:default e {:oi s})))

(defn req->query
  [req]
  (merge
   {:per-page +per-page+ :page 0}
   (if-let [obj (get-in req [:params "object"])]
     (string->object obj))
   (updates
    (set/rename-keys
     (clojure.walk/keywordize-keys
      (select-keys
       (:params req)
       ["graph" "subject" "predicate" "per-page" "page"]))
     {:graph :gi :subject :si :predicate :pi})
    :per-page #(min +per-page+ (max 1 (str->int % +per-page+)))
    :page #(max 0 (str->int % 0)))))

;;;;; Query handling
(defn matches-query?
  [query entry]
  (every?
   identity
   (map (fn [k]
          (or (nil? (get query k))
              (= (get query k) (get entry k))))
        [:gi :si :pi :oi :ol :ln :di])))

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
  (paginated (get query :per-page +per-page+) (get query :page 0)
             (filter (partial matches-query? query) data)))

(defn -query->sql-where-clause
  [query]
  (let [relevant (remove-falsies (select-keys query [:gi :si :pi :oi :ol :ln :di]))]
    (when (not (empty? relevant))
      (cons
       (str " WHERE " (string/join " AND " (remove nil? (map #(str (name %) "=?") (keys relevant)))))
       (vals relevant)))))

(defn -query->sql-pagination [{:keys [per-page page]}]
  (str " " (string/join
            " "
            (remove
             nil? [(str "LIMIT " per-page)
                   (when (> page 0) (str "OFFSET " (* page per-page)))]))))

(defn query->sql
  [query & {:keys [count?] :or {count false}}]
  (let [base (if count? "SELECT COUNT(*) FROM ontology" "SELECT * FROM ontology")
        [where & params] (-query->sql-where-clause query)
        pagination (-query->sql-pagination query)]
    (vec
     (cons
      (string/join [base where pagination])
      params))))

(defmethod query :database ;; if we've got a database, we need to query it for matching entries
  [query data]
  (sql/with-db-connection [db data]
    (let [ct (second (first (first (sql/query db (query->sql query :count? true)))))]
      {:total ct :per-page (:per-page query) :page (:page query)
       :items (vec (map #(into {} (filter second %)) (sql/query db (query->sql query))))})))
