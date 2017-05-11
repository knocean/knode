(ns knode.cli
  (:require
   [clojure.java.io :as io]
   [clojure.string :as string]
   [clojure.data.json :as json]

   [knode.state :refer [state]]
   [knode.core :as core]
   [knode.emit :as emit]
   [knode.server :as server]
   [knode.sparql :as sparql])
  (:gen-class))

; TODO: This is a big mess. The TSV code should be generalized.

(defn load-state!
  "Given a directory, load data into the atoms."
  [dir project-name]
  (let [path (str dir "context.kn")]
    (when (.exists (io/file path))
      (with-open [reader (io/reader path)]
        (let [[env blocks] (core/process-lines {} (line-seq reader))]
          (swap! state assoc :context blocks)
          (swap! state assoc :env env)))))
  (let [path (str dir "external.tsv")]
    (when (.exists (io/file path))
      (with-open [reader (io/reader path)]
        (doseq [line (->> reader line-seq rest)]
          (let [[label curie _] (string/split line #"\t" 3)
                target (core/resolve-name (:env @state) {:curie curie})
                new-env (core/add-label (:env @state) label target nil)]
            (swap! state assoc :env new-env))))))
  (let [path (str dir "index.tsv")]
    (when (.exists (io/file path))
      (with-open [reader (io/reader path)]
        (doseq [line (->> reader line-seq rest)]
          (let [[curie label _] (string/split line #"\t" 3)
                target (core/resolve-name (:env @state) {:curie curie})
                new-env (core/add-label (:env @state) label target nil)]
            (swap! state assoc :env new-env))))))
  (let [path (str dir "predicates.tsv")]
    (when (.exists (io/file path))
      (with-open [reader (io/reader path)]
        (doseq [line (->> reader line-seq rest)]
          (let [[label curie datatype] (string/split line #"\t")
                target (core/resolve-name (:env @state) {:curie curie})
                dt (core/resolve-datatype (:env @state) {:name datatype})
                new-env (core/add-label (:env @state) label target dt)]
            (swap! state assoc :env new-env))))))
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

;; (swap! state #(assoc % :root-dir "/home/inaimathi/projects/ONTIE/" :ontology-dir "/home/inaimathi/projects/ONTIE/ontology/" :project-name "ontie"))

;; TODO: test command
(defn -main [task & args]
  (case task
    "configuration" (println (knode.state/report @state))
    "serve" (do (load!)
                (sparql/init-dataset! state)
                (server/serve))
    "reindex" (do (load!)
                  (spit
                   (str (:root-dir @state) "ontology/index.tsv")
                   (emit/emit-index (:env @state) (:terms @state))))
    "load-ncbi" (do (sparql/init-dataset! state)
                    (sparql/load-taxa! @state "tmp/taxdmp.zip"))
    "validate" (do (load!)
                   (sparql/init-dataset! state)
                   (sparql/load-terms! @state)
                   (let [rules (sparql/validate-each @state 1)]
                     (when (->> rules (filter :results) first)
                       (doseq [rule rules]
                         (println
                          (if (-> rule :results first) "FAIL" "PASS")
                          (:label rule))
                         (doseq [result (:results rule)]
                           (->> result
                                :values
                                (string/join " | ")
                                (println "    "))))
                       (println "VALIDATION FAILED")
                       (System/exit 1))))
    "test" (println "TODO")))
