(ns com.knocean.knode.cli
  (:require [com.knocean.knode.state :as st]
            [com.knocean.knode.server :as server])
  (:gen-class))

(def usage "Usage: knode TASK

  knode serve    Serve resources
  knode config   Show configuration
  knode help     Show this help message")

(defn -main [& args]
  (let [task (first args)]
    (case task
      "serve" (server/start)
      "config" (println (st/report @st/state))
      "help" (println usage)
      (do (println "Unknown task:" task)
          (println usage)
          (System/exit 1)))))
