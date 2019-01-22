(ns com.knocean.knode.pages.ontology.tsv
  (:require [clojure.string :as string]
            [clojure.java.jdbc :as jdbc]

            [org.knotation.rdf :as rdf]
            [org.knotation.environment :as en] 

            [com.knocean.knode.util :as util]
            [com.knocean.knode.pages.ontology.base :refer [ontology-result] :as base]
            [com.knocean.knode.state :refer [state] :as st]))

(defn tsv-join
  [seq]
  (->> seq
       (map (partial string/join \tab))
       (map #(str % \newline))
       string/join))

(defn render-object
  [env format {:keys [oi ob ol] :as state}]
  (cond
    oi (case format
         :CURIE (en/iri->curie env oi)
         :label (en/iri->name env oi)
         oi)
    ob ob
    ol ol
    :else ""))

(defn iri->seq
  [env headers iri]
  (let [states (st/select [:= :si iri] :order-by :id)]
    (for [[column pi format] headers]
      (case column
        "IRI" iri
        "CURIE" (en/iri->curie env iri)
        "recognized" (->> states first boolean str)
        (->> states
             (filter #(= pi (:pi %)))
             (map (partial render-object env format))
             distinct
             (string/join "|"))))))

(defn parse-tsv-select
  "Given a query string, return a vector of required columns."
  [env compact? select]
  (let [format-reg #" \[(CURIE|IRI|label)\]$"
        default (if compact? :CURIE :IRI)]
    (->> (string/split select #",")
         (map
          (fn [k]
            (if-let [format (second (re-find format-reg k))]
              [k (util/->iri env (string/replace k format-reg "")) (keyword format)]
              (cond
                (= k "IRI") ["IRI" nil :IRI]
                (= k "CURIE") ["CURIE" nil :CURIE]
                (= (util/->iri env k) (rdf/rdfs "label"))
                [k (rdf/rdfs "label") :literal]
                :else [k (util/->iri env k) default]))))
         vec)))

(defmethod ontology-result "tsv"
  [{:keys [requested-iris params env] :as req}]
  {:status 200
   :body
   (let [show-headers? (not (= (get params "show-headers") "false"))
         compact? (= (get params "compact") "true")
         curie-column (when (:body-string req)
                        (->> req :body-string string/split-lines first string/trim (= "CURIE")))
         default (if compact? :CURIE :IRI)
         headers (if-let [select (get params "select")]
                   (parse-tsv-select env compact? select)
                   [(if (or compact? curie-column) ["CURIE" nil :CURIE] ["IRI" nil :IRI])
                    ["label" (rdf/rdfs "label") :label]
                    ["recognized" nil :boolean]
                    ["obsolete" (rdf/owl "deprecated") :boolean]
                    ["replacement" "http://purl.obolibrary.org/obo/IAO_0100001" default]])
         iris (if (first requested-iris)
                requested-iris
                (base/all-subjects))
         rows (map (partial iri->seq env headers) iris)]
     (tsv-join
      (if show-headers?
        (cons (map first headers) rows)
        rows)))})
