(ns knode.cli
  (:require
   [clojure.java.io :as io]
   [clojure.string :as string]

   [knode.core :as core]
   [knode.server :as server]
   [knode.state :as state])
  (:gen-class))

(defn load-state!
  "Given a directory, load data into the atoms."
  [dir]
  (with-open [reader (io/reader (str dir "context.kn"))]
    (let [[env blocks] (core/process-lines {} (line-seq reader))]
      (swap! state/state assoc :context blocks)
      (swap! state/state assoc :env env)))
  (with-open [reader (io/reader (str dir "external.tsv"))]
    (->> reader
         line-seq
         rest
         (map #(string/split % #"\t"))
         (map (fn [[label target _]] (str "@label " label ": " target)))
         (core/process-lines (:env @state/state))
         first
         (swap! state/state assoc :env)))
  (with-open [reader (io/reader (str dir "index.tsv"))]
    (->> reader
         line-seq
         rest
         (map #(string/split % #"\t" 3))
         (map (fn [[curie label _]] (str "@label " label ": " curie)))
         (core/process-lines (:env @state/state))
         first
         (swap! state/state assoc :env)))
  (with-open [reader (io/reader (str dir "ontie.kn"))]
    (->> (core/process-lines (:env @state/state) (line-seq reader))
         second
         (partition-by :subject)
         (partition 2)
         (map
          (fn [[[subject] blocks]]
            [(get-in subject [:subject :iri])
             {:subject subject
              :blocks blocks}]))
         (into {})
         (swap! state/state assoc :terms))))

;; TODO: test command
(defn -main [task dir root port & args]
  (reset! state/root-iri root)
  (case task
    "serve" (do (println "Loading data from" dir "...")
                (load-state! dir)
                (server/serve dir port))
    "test" (println "TODO")))
