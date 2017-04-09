(ns knode.cli
  (:require [knode.core :as core])
  (:gen-class))

(defn serve [port directory]
  (println "TODO - serve the given kn files in html/JSON/ttl format"))

(defn test [source-dir expected-dir]
  (println "TODO - slurp files from source dir, check that the outputs match expected-dir"))

(defn -main [task & args]
  (println "TODO - " task args))
