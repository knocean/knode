(ns com.knocean.knode.cli
  (:require [com.knocean.knode.state :as st]
            [com.knocean.knode.server :as server]
            [com.knocean.knode.loader :as loader])
  (:gen-class))

(def usage "Usage: knode TASK

  knode serve    Serve resources
  knode config   Show configuration
  knode load [resource] [files]
  knode db (create|drop) (tables|indexes)
  knode help     Show this help message")

(defn -main [& args]
  (let [task (first args)]
    (case task
      "help" (println usage)
      "config" (do (st/init) (println (st/report @st/state)))
      "serve" (do (st/init) (server/start))

      "load" (do (st/init) (loader/load-resource (second args) (drop 2 args)))
      "db" (do (st/init)
               (case (->> args rest (take 2))
                 ["create" "tables"] (loader/create-tables)
                 ["drop" "tables"] (loader/drop-tables)
                 ["create" "indexes"] (loader/create-indexes)
                 ["drop" "indexes"] (loader/drop-indexes)
                 (do (apply println "ERROR: Unknown db tasks:" (->> args rest (take 2)))
                     (println usage)
                     (System/exit 1))))

      (do (println (format "ERROR: Unknown task '%s'" task))
          (println usage)
          (System/exit 1)))))
