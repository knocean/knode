(ns knode.cli
  (:require
   [clojure.java.io :as io]
   [clojure.string :as string]

   [knode.state :refer [state]]
   [knode.core :as core]
   [knode.server :as server])
  (:gen-class))

(defn load-state!
  "Given a directory, load data into the atoms."
  [dir project-name]
  (with-open [reader (io/reader (str dir "context.kn"))]
    (let [[env blocks] (core/process-lines {} (line-seq reader))]
      (swap! state assoc :context blocks)
      (swap! state assoc :env env)))
  (with-open [reader (io/reader (str dir "external.tsv"))]
    (->> reader
         line-seq
         rest
         (map #(string/split % #"\t"))
         (map (fn [[label target _]] (str "@label " label ": " target)))
         (core/process-lines (:env @state))
         first
         (swap! state assoc :env)))
  (with-open [reader (io/reader (str dir "index.tsv"))]
    (->> reader
         line-seq
         rest
         (map #(string/split % #"\t" 3))
         (reduce
          (fn [env [target label _]]
            (core/add-label env label target nil))
          (:env @state))
         (swap! state assoc :env)))
  (with-open [reader (io/reader (str dir project-name ".kn"))]
    (->> (core/process-lines (:env @state) (line-seq reader))
         second
         (partition-by :subject)
         (partition 2)
         (map
          (fn [[[subject] blocks]]
            [(get-in subject [:subject :iri])
             {:subject (:subject subject)
              :blocks blocks}]))
         (into {})
         (swap! state assoc :terms))))

;; TODO: test command
(defn -main [task & args]
  (case task
    "serve" (let [dir (str (:root-dir @state) "ontology/")]
              (println "Loading data from" dir "...")
              (load-state! dir (:project-name @state))
              (server/serve))
    "test" (println "TODO")))
