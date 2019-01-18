(ns com.knocean.knode.loader
  (:require [clojure.java.io :as io]
            [clojure.string :as s]
            [org.knotation.clj-api :as kn]
            [org.knotation.jena :as jena]
            [org.knotation.rdf :as rdf]
            [org.knotation.state :as knst]
            [com.knocean.knode.state :refer [state] :as st]
            [com.knocean.knode.resources :as r]))

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

(defn load-states
  [id fpath]
  (->> (kn/read-path :kn nil fpath)
       st/get-quads
       (map #(assoc % :rt id))
       (partition-all 2000)
       (map-indexed
         (fn [i p]
           (println "Loading partition" i)
           (st/insert! id p)))
       doall))

(defn load-branch-states
  [dir {:keys [:paths] as :resource}]
  (doseq [p paths]
    (let [id (first (s/split p #"\."))
          fpath (str dir id ".kn")]
      (load-states id fpath))))

(defn load-resource
  ([resource]
    (println "Loading resource" (:idspace resource))
    ; add the resource to the 'resources' table
    (try
    (st/query {:select [:*] :from [:resources] :limit 1})
    (catch Exception e
        (println "The 'resources' table does not exist... Creating...")
        (create-tables)))
    (if (= (:type resource) :obo-github-repo)
      ;; add a resource for each branch
      (r/insert-branches! resource)
      (r/insert! resource))
    ; then add the states for the resource to the 'states' table
    (try
      (st/query {:select [:*] :from [:states] :limit 1})
      (catch Exception e
        (println "The 'states' table does not exist... Creating...")
        (create-tables)))

    ; get the ontology directory and resource file path
    (let [dir (if (s/ends-with? (:absolute-dir @state) "/")
                    (str (:absolute-dir @state) "ontology/")
                    (str (:absolute-dir @state) "/ontology/"))
          ; all resources should be in KN
          fpath (str dir (:idspace resource) ".kn")]
      (if (= (:type resource) :obo-github-repo)
        (load-branch-states dir resource)
        (load-states fpath (:idspace resource)))))

  ([resource args]
   (println "LOAD" (:connection @state) resource args)
   (try
     (st/query {:select [:*] :from [:states] :limit 1})
     (catch Exception e
       (println "The 'states' table does not exist.")
       (create-tables)))
   (->> (if (second args)
          (kn/read-paths nil nil args)
          (kn/read-path nil nil (first args)))
        st/get-quads
        (map #(assoc % :rt resource))
        (partition-all 2000) ; WARN: This number is pretty arbitrary
        (map-indexed
         (fn [i p]
           (println "Loading partition" i)
           (st/insert! resource p)))
        doall)))
