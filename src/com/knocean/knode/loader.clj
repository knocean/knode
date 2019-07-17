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

(defn create-resources-table
  []
  (println "Creating resources table...")
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
      "postgres" "serial PRIMARY KEY"))))

(defn create-states-table
  []
  (println "Creating table states...")
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
  lt text
);"
    (case (:database-type @state)
      "sqlite" "integer PRIMARY KEY AUTOINCREMENT"
      "postgres" "serial PRIMARY KEY"))))

(defn create-tables
  []
  (create-resources-table)
  (create-states-table))

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
  [id states]
  (->> states
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
   (println "LOAD" (:idspace resource))
   ; add the resource to the 'resources' table
   (try
     (st/query {:select [:*] :from [:resources] :limit 1})
     (catch Exception e
         (create-resources-table)))
   (if (= (:type resource) :obo-github-repo)
     ;; add a resource for each branch
     (r/insert-branches! resource)
     (r/insert! resource))
   ; then add the states for the resource to the 'states' table
   (try
     (st/query {:select [:*] :from [:states] :limit 1})
     (catch Exception e
       (create-states-table)))
   ; get the ontology directory and resource file path
   (let [dir (if (s/ends-with? (:absolute-dir @state) "/")
                (str (:absolute-dir @state) "ontology/")
                (str (:absolute-dir @state) "/ontology/"))
         id (:idspace resource)
         fpath (:path resource)
         paths (:paths resource)]
     (if (= (:type resource) :obo-github-repo)
       (load-branch-states dir resource)
       (if (not (nil? paths))
        (load-states id (kn/read-paths nil nil paths))
        (load-states id (kn/read-path nil nil fpath))))))

  ([resource args]
   (println "LOAD" (:connection @state) resource args)
   (try
     (st/query {:select [:*] :from [:resources] :limit 1})
     (catch Exception e
         (create-resources-table)))
   (r/insert! {:idspace resource :label resource :type :local-file})
   (try
     (st/query {:select [:*] :from [:states] :limit 1})
     (catch Exception e
       (create-states-table)))
   (->> (if (second args)
          (kn/read-paths nil nil args)
          (kn/read-path nil nil (first args)))
        (load-states resource))))
