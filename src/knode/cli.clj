(ns knode.cli
  (:require
   [clojure.java.io :as io]
   [clojure.string :as string]

   [knode.core :as in]
   [knode.emit :as out])
  (:gen-class))

(defn add-tsv-to-env+forms [env+forms filename]
  (with-open [reader (io/reader filename)]
    (doall
     (let [lns (map #(string/split % #"\t") (line-seq reader))
           headers (map #(keyword (string/lower-case %)) (first lns))]
       (in/add-labels-to-env+forms
        env+forms (map #(into {} (map vector headers %)) (rest lns)))))))

(defn serve [port directory]
  (println "TODO - serve the given kn files in html/JSON/ttl format"))

(defn test [source-dir expected-dir]
  (println "TODO - slurp files from source dir, check that the outputs match expected-dir"))

(defn -main [task & args]
  (println "TODO - " task args))
