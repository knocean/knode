(ns knode.state
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [environ.core :refer [env]]

            [knode.core :as core]))

(def configurators
  [["Root dir" (constantly "")]
   ["Project name" #(->> (:root-dir %)
                         io/file
                         .getAbsolutePath
                         io/file
                         .getName
                         string/lower-case)]
   ["IDSPACE" #(string/upper-case (:project-name %))]
   ["Port" (constantly "3210")]
   ["Root IRI" #(str "http://localhost:" (:port %) "/")]
   ["Project IRI" #(str (:root-iri %) "ontology/" (:idspace %))]
   ["Term IRI format" #(str (:project-iri %) "_%07d")]])

(defn label->keyword
  [label]
  (-> label
      string/lower-case
      (string/replace #"\W+" "-")
      keyword))

(defn init
  [base]
  (reduce
   (fn [state [label f]]
     (let [k (label->keyword label)]
       (if (find state k) state (assoc state k (f state)))))
   (->> configurators
        (map first)
        (map label->keyword)
        (concat [:dev-key])
        (select-keys base))
   configurators))

(defn report
  [state]
  (->> (concat
        (->> configurators
             (map first)
             (map (juxt #(str % ":") #(get state (label->keyword %)))))
        [["Dev key set?" (-> state :dev-key string/blank? not)]])
       (map (fn [[k v]] (format "%-17s %s" k (str v))))
       (string/join "\n")))

(def state (atom (init env)))

(defn obsolete-term? [term]
  (if-let [obs (first (filter #(= "obsolete" (get-in % [:predicate :label])) (:blocks term)))]
    (= "true" (:content obs))
    false))

(defn replacement-for [term]
  (if-let [rep (first (filter #(= "http://purl.obolibrary.org/obo/IAO_0000118" (get-in % [:predicate :iri])) (:blocks term)))]
    (get-in rep [:object :iri])))

(defn query-term-graph [graph term-iri]
  (println "TODO")
  nil)

(defn expanded-iri-or-curie [iri-or-curie]
  (or (if-let [parsed (core/parse-curie iri-or-curie)]
        (try
          (:iri (core/resolve-curie env parsed))
          (catch Exception e nil)))
      iri-or-curie))

(defn term-status
  [iri-or-curie & {:keys [graph terms-table label]
                   :or {env (:env @state)
                        terms-table (:terms @state)
                        graph (:graph @state)
                        label :iri}}]
  (let [expanded (expanded-iri-or-curie iri-or-curie)]
    (merge {label iri-or-curie :recognized true :obsolete false :replacement nil}
           (if-let [term (or (get terms-table expanded)
                             (and graph (query-term-graph graph expanded)))]
             (when (obsolete-term? term)
               {:obsolete true :replacement (replacement-for term)})
             {:recognized false}))))
