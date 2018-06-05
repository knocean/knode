(ns com.knocean.knode.loader
  (:require [clojure.java.io :as io]
            [org.knotation.clj-api :as kn]
            [org.knotation.jena :as jena]
            [com.knocean.knode.state :refer [state] :as st]))

(defn drop-tables
  []
  (println "Dropping tables...")
  (st/execute!
   "DROP TABLE IF EXISTS resources;
DROP TABLE IF EXISTS states;"))

(defn create-tables
  []
  (println "Creating tables...")
  (st/execute!
   (format
    "CREATE TABLE resources (
  id %s,
  label text,
  title text,
  description text,
  homepage text
);"
    (case (:database-type @state)
      "sqlite" "integer PRIMARY KEY AUTOINCREMENT"
      "postgres" "serial PRIMARY KEY")))
  (st/execute!
   (format
    "CREATE TABLE states (
  id %s,
  rt text,
  gi text,
  si text,
  sb text,
  pi text,
  oi text,
  ob text,
  ol text,
  di text,
  ln text
);"
    (case (:database-type @state)
      "sqlite" "integer PRIMARY KEY AUTOINCREMENT"
      "postgres" "serial PRIMARY KEY"))))

(defn drop-indexes
  []
  (println "Dropping indexes...")
  (st/execute!
   "DROP INDEX IF EXISTS states_rt;
DROP INDEX IF EXISTS states_si;"))

(defn create-indexes
  []
  (println "Creating indexes...")
  (st/execute!
   "CREATE INDEX states_rt ON states(rt);
CREATE INDEX states_si ON states(si);"))

(defn load-resource
  [resource args]
  (println "LOAD" (:connection @state) resource args)
  (try
    (st/query {:select [:*] :from [:states] :limit 1})
    (catch Exception e
      (println "The 'states' table does not exist.")
      (create-tables)))
  (->> (if (second args)
         (kn/read-paths nil nil args)
         (kn/read-path nil nil (first args)))
       (filter :pi)
       (map #(assoc % :rt resource))
       (partition-all 2000) ; WARN: This number is pretty arbitrary
       (map-indexed
        (fn [i p]
          (println "Loading partition" i)
          (st/insert! p)))
       doall))
