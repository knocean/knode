(ns com.knocean.knode.linked-data-fragments
  (:require [clojure.edn :as edn]

            [org.knotation.util :as util]
            [org.knotation.rdf :as rdf]
            [org.knotation.object :as ob]))

(defn tap!
  ([v] (tap! "TAPPED" v))
  ([label v]
   (println label (str v))
   v))

(defn remove-falsies
  [sequence]
  (into {} (filter second sequence)))

(defn updates
  [m & {:as k-f-map}]
  (reduce
   (fn [memo [k f]]
     (update memo k f))
   m k-f-map))

(defn str->int
  ([str] (str->int str 0))
  ([str default]
   (util/handler-case
    (Integer/parseInt str)
    (:default e default))))

;;;;; Query parsing
(defn string->object
  [s]
  (util/handler-case
   (let [res (ob/nquads-object->object s)]
     (remove-falsies
      [[:iri (::rdf/iri res)]
       [:label (::rdf/lexical res)]
       [:language (::rdf/language res)]
       [:datatype (::rdf/datatype res)]]))
   (:default e {:iri s})))

(defn req->query
  [req]
  (merge
   {:per-page 100 :pg 0}
   (updates
    (clojure.walk/keywordize-keys
     (select-keys
      (:params req)
      ["graph" "subject" "predicate" "object" "per-page" "pg"]))
    :object #(when % (string->object %))
    :per-page #(max 1 (str->int % 100))
    :pg #(max 0 (str->int % 0)))))

;;;;; Query handling
(defn ?= [a b] (or (nil? a) (= a b)))

(defn matches-query?
  [{:keys [graph subject predicate object] :as query} entry]
  (and (?= graph (:gi entry))
       (?= subject (:si entry))
       (?= predicate (:pi entry))
       (or (nil? object)
           (and (:iri object) (= (:iri object) (:oi entry)))
           (and (= (:label object) (:ol entry))
                (?= (:language object) (:ln entry))
                (?= (:datatype object) (:di entry))))))

(defn paginated [per-page pg seq]
  {:total (count seq)
   :per-page per-page
   :page pg
   :items (vec (take per-page (drop (* per-page pg) seq)))})

(defmulti query #(type %2)) ;; we'll want to expand this to SQL datastores from the sound of it
(defmethod query :default ;; default is an in-memory sequence, which we just filter.
  [query data]
  (paginated (get query :per-page 10) (get query :pg 0)
             (filter (partial matches-query? query) data)))
