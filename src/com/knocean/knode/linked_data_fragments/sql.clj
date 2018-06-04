(ns com.knocean.knode.linked-data-fragments.sql
  (:require [clojure.edn :as edn]
            [clojure.string :as string]
            [clojure.set :as set]
            [clojure.java.jdbc :as jdbc]
            [honeysql.core :as sql]

            [org.knotation.util :as util]
            [org.knotation.rdf :as rdf]

            [com.knocean.knode.state :refer [state] :as st]
            [com.knocean.knode.linked-data-fragments.base :as base :refer [query query-stream]]))

;;; DUMMY DATA
;; (def db
;;   {:classname "org.sqlite.JDBC"
;;    :subprotocol "sqlite"
;;    :subname "resources/obi_core.db"})
;; (defn dummy-db!
;;   [db]
;;   (jdbc/with-db-connection [handle db]
;;     (jdbc/execute! handle ["drop table if exists ontology"])
;;     (jdbc/execute! handle [(jdbc/create-table-ddl
;;                            :ontology
;;                            (map (fn [name] [name :string])
;;                                 [:gi :si :sb :pi :oi :ob :ol :di :ln]))])
;;     (doseq [d (:maps @st/state)] (jdbc/insert! handle :ontology d))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def columns [:gi :si :sb :pi :oi :ol :ln :di])

(defn query->sql
  [{:keys [per-page page] :as query}]
  (let [wheres (map (fn [[k v]] [:= k v]) (select-keys query columns))]
    (sql/build
     :select :* :from :states
     :where (when (not (empty? wheres)) (vec (cons :and wheres)))
     :limit per-page :offset (when (> page 0) (* page per-page)))))

(defmethod query-stream :database
  [{:keys [per-page page] :as query} source]
  (let [wheres (map (fn [[k v]] [:= k v]) (select-keys query columns))]
    (map
     #(into {} (filter second %))
     (jdbc/query
      source
      (sql/format
       (dissoc (query->sql query) :limit :offset))))))

(defn count!
  [{:keys [per-page page] :as query} db]
  (let [wheres (map (fn [[k v]] [:= k v]) (select-keys query columns))]
    (second
     (first
      (first
       (jdbc/query
        db (sql/format (assoc (query->sql query) :select [:%count.*]))))))))

(defmethod query :database ;; if we've got a database, we need to query it for matching entries
  [query data]
  (jdbc/with-db-connection [db data]
    {:total (count! query db) :per-page (:per-page query) :page (:page query)
     :items (vec (map #(into {} (filter second %))
                      (jdbc/query db (sql/format (query->sql query)))))}))
