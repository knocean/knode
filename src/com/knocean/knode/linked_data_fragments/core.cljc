(ns com.knocean.knode.linked-data-fragments.core
  (:require [clojure.edn :as edn]
            [clojure.string :as string]
            [clojure.set :as set]

            [org.knotation.util :as util]
            [org.knotation.rdf :as rdf]))

(defn nquads-object->object
  [object]
  (throw (Exception. "TODO: object->nquads-object")))

(def +per-page+ 1000)

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
   (let [res (nquads-object->object s)]
     (remove-falsies
      [[:oi (::rdf/iri res)]
       [:ol (::rdf/lexical res)]
       [:ln (::rdf/language res)]
       [:di (::rdf/datatype res)]]))
   (:default e {:oi s})))

(defn req->query
  [req]
  (merge
   {:per-page +per-page+ :page 0}
   (if-let [obj (get-in req [:params "object"])]
     (string->object obj))
   (updates
    (set/rename-keys
     (clojure.walk/keywordize-keys
      (select-keys
       (:params req)
       ["graph" "subject" "predicate" "per-page" "page"]))
     {:graph :gi :subject :si :predicate :pi})
    :per-page #(min +per-page+ (max 1 (str->int % +per-page+)))
    :page #(max 0 (str->int % 0)))))

;;;;; Query handling
(defn matches-query?
  [query entry]
  (let [blanks {:si :sb :oi :ob}]
    (every?
     identity
     (map (fn [k]
            (let [q (get query k)]
              (or (nil? q)
                  (= q (get entry k))
                  (and (= ":blank" q) (get entry (get blanks k))))))
          [:gi :si :pi :oi :ol :ln :di]))))

(defn paginated [per-page pg seq]
  {:total (count seq)
   :per-page per-page
   :page pg
   :items (vec (take per-page (drop (* per-page pg) seq)))})

(defmulti query
  #(cond
     (or (vector? %2) (list? %2)) :default

     (and (map? %2)
          (set/subset?
           (set (keys %2))
           #{:classname :subprotocol :subname :connection}))
     :database

     :else :default))

(defmethod query :default ;; default is an in-memory sequence, which we just filter.
  [query data]
  (paginated (get query :per-page +per-page+) (get query :page 0)
             (filter (partial matches-query? query) data)))
