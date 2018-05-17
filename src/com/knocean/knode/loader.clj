(ns com.knocean.knode.loader
  (:require [clojure.java.io :as io]
            [clojure.java.jdbc :as jdbc]
            [org.knotation.clj-api :as kn]
            [org.knotation.jena :as jena]
            [com.knocean.knode.state :refer [state]]))

(defn drop-tables
  []
  (println "Dropping tables...")
  (jdbc/execute!
   @state
   "DROP TABLE IF EXISTS states;"))

(defn create-tables
  []
  (println "Creating tables...")
  (jdbc/execute!
   @state
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
  dt text,
  ln text
);"
    (case (:database-type @state)
      "sqlite" "integer PRIMARY KEY AUTOINCREMENT"
      "postgres" "serial PRIMARY KEY"))))

(defn drop-indexes
  []
  (println "Dropping indexes...")
  (jdbc/execute!
   @state
   "DROP INDEX IF EXISTS states_rt;
DROP INDEX IF EXISTS states_si;"))

(defn create-indexes
  []
  (println "Creating indexes...")
  (jdbc/execute!
   @state
   "CREATE INDEX states_rt ON states(rt);
CREATE INDEX states_si ON states(si);"))

(defn load-resource
  [resource args]
  (println "LOAD" (:connection @state) resource args)
  (try
    (jdbc/query @state "SELECT * FROM states LIMIT 1")
    (catch Exception e
      (println "The 'states' table does not exist.")
      (create-tables)))
  (->> (if (second args)
         (kn/read-paths nil nil args)
         (kn/read-path nil nil (first args)))
       (filter :pi)
       (map (juxt (constantly resource) :gi :si :sb :pi :oi :ob :ol :dt :ln))
       (partition-all 2000) ; WARN: This number is pretty arbitrary
       (map-indexed
        (fn [i p]
          (println "Loading partition" i)
          (jdbc/insert-multi!
           @state
           "states"
           [:rt :gi :si :sb :pi :oi :ob :ol :dt :ln]
           p)))
       doall))
