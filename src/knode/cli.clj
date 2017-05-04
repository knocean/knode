(ns knode.cli
  (:require
   [clojure.java.io :as io]
   [clojure.string :as string]
   [clojure.data.json :as json]

   [knode.state :refer [state]]
   [knode.core :as core]
   [knode.server :as server]
   [knode.sparql :as sparql])
  (:gen-class))

; TODO: This is a big mess. The TSV code should be generalized.

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
    (doseq [line (->> reader line-seq rest)]
      (let [[curie label _] (string/split line #"\t" 3)
            target (core/resolve-name (:env @state) {:curie curie})
            new-env (core/add-label (:env @state) label target nil)]
        (swap! state assoc :env new-env))))
  (let [path (str dir "templates.json")]
    (when (.exists (io/file path))
      (->> (json/read-str (slurp path) :key-fn keyword)
           (map
            (fn [template]
              (assoc
               template
               :statements
               (map (partial core/resolve-block (:env @state))
                    (:statements template)))))
           (map (juxt :iri identity))
           (into {})
           (swap! state assoc :templates)))
    (doseq [{:keys [label iri]} (->> @state :templates vals)]
      (let [new-env (core/add-label (:env @state) label {:iri iri} nil)]
        (swap! state assoc :env new-env))))
  (let [path (str dir "validation.edn")]
    (when (.exists (io/file path))
      (->> (slurp path)
           clojure.edn/read-string
           (swap! state assoc :validation-rules))))
  (with-open [reader (io/reader (str dir project-name ".kn"))]
    (->> (core/process-lines (:env @state) (line-seq reader))
         second
         (partition-by :subject)
         (partition 2)
         (map
          (fn [[[subject] blocks]]
            [(get-in subject [:subject :iri])
             {:subject (:subject subject)
              :blocks
              (core/expand-templates
               (:env @state)
               (:templates @state)
               blocks)}]))
         (into {})
         (swap! state assoc :terms))))

(defn load!
  []
  (println "Loading from" (:ontology-dir @state) "...")
  (load-state! (:ontology-dir @state) (:project-name @state)))

;; TODO: test command
(defn -main [task & args]
  (case task
    "configuration" (println (knode.state/report @state))
    "serve" (do (load!)
                (server/serve))
    "load-ncbi" (do (sparql/init-dataset! state)
                    (sparql/load-taxa! @state "taxdmp.zip"))
    "validate" (do (load!)
                   (sparql/init-dataset! state)
                   (sparql/load-terms! @state)
                   (println "Running validation...")
                   (let [results (sparql/validate @state)]
                     (println results)))
    "test" (println "TODO")))
