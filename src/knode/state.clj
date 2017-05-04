(ns knode.state
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [environ.core :refer [env]]))

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
