(ns com.knocean.knode.pages.ontology.tsv
  (:require [clojure.string :as string]

            [org.knotation.link :as ln]
            [org.knotation.rdf :as rdf]
            [org.knotation.environment :as en]

            [com.knocean.knode.pages.ontology.base :refer [ontology-result] :as base]
            [com.knocean.knode.state :refer [state] :as st]
            [com.knocean.knode.pages.term-status :as stat]))

(defn tap!
  ([thing] (tap! "TAPPED" thing))
  ([label thing] (println label thing) thing))

(defn node->tsv
  [env node]
  (let [s (stat/term-status (::rdf/iri node))]
    (assoc s :label (get-in env [::en/iri-label (:IRI s)]))))

(defn tsv-join [seq]
  (string/join \newline (map #(string/join \tab %) seq)))

(defn nodes->seqs
  [env headers nodes & {:keys [compact?] :or {compact false}}]
  (map
   #(map
     (fn [[k format]]
       (let [val (cond
                   (= :CURIE k) [(:IRI %)]
                   (keyword? k) [(get % k)]
                   (string? k) (get % (ln/subject->iri env k)))
             iris (map (fn [v] (or (ln/subject->iri env v)
                                   (ln/subject->iri env (::rdf/lexical v))
                                   v)) val)
             collapsed (fn [f]
                         (map
                          (fn [v]
                            (cond (or (boolean? v) (empty? v)) (str v)
                                  (and (map? v) (::rdf/lexical v)) (::rdf/lexical v)
                                  :else (f env v)))
                          iris))
             formatted (vec
                        (case format
                          :IRI iris
                          :CURIE (collapsed ln/iri->curie)
                          :label (collapsed ln/iri->name)))]
         (string/join "|" (map str formatted))))
     headers)
   nodes))

(defn parse-tsv-select
  [env compact? select]
  (let [format-reg #" \[(CURIE|IRI|label)\]$"]
    (->> (string/split select #",")
         (map (fn [k]
                (if-let [format (second (re-find format-reg k))]
                  [(string/replace k format-reg "") (keyword format)]
                  (cond
                    (= k "IRI") [:IRI :IRI]
                    (= k "CURIE") [:CURIE :CURIE]
                    (= k "recognized") [:recognized :label]
                    (= k "obsolete") [:obsolete :label]
                    (= (ln/subject->iri env k) "http://www.w3.org/2000/01/rdf-schema#label")
                      [k :label]
                    :else [k (if compact? :CURIE :label)]))))
         vec)))

(defmethod ontology-result "tsv"
  [{:keys [requested-iris params env] :as req}]
  {:status 200
   :body (let [show-headers? (not (= (get params "show-headers") "false"))
               compact? (= (get params "compact") "true")
               headers (if-let [sel (get params "select")]
                         (parse-tsv-select env compact? sel)
                         (if compact?
                           [[:CURIE :CURIE] ["label" :label] [:recognized :label] [:obsolete :label]]
                           [[:IRI :IRI] ["label" :label] [:recognized :label] [:obsolete :label]]))
               nodes (map (partial node->tsv env)
                          (if (not (empty? requested-iris))
                            (map (fn [iri] {::rdf/iri iri}) requested-iris)
                            (base/all-subjects)))
               rows (nodes->seqs env headers nodes :compact? compact?)]
           (tsv-join
            (if show-headers?
              (cons (->> headers (map first) (map #(if (keyword? %) (name %) %))) rows)
              rows)))})
