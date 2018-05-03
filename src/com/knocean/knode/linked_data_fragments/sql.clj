(ns com.knocean.knode.linked-data-fragments.sql
  (:require [clojure.edn :as edn]
            [clojure.string :as string]
            [clojure.set :as set]
            [clojure.java.jdbc :as sql]

            [org.knotation.util :as util]
            [org.knotation.rdf :as rdf]
            [org.knotation.object :as ob]

            [com.knocean.knode.linked-data-fragments.core :refer [query remove-falsies]]))

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
