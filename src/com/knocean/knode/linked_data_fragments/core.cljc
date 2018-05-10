(ns com.knocean.knode.linked-data-fragments.core
  (:require [clojure.edn :as edn]
            [clojure.string :as string]
            [clojure.set :as set]

            [org.knotation.util :as util]
            [org.knotation.rdf :as rdf]
            [org.knotation.object :as ob]

            [com.knocean.knode.linked-data-fragments.base :as base :refer [query query-stream]]))

;;;;; Query parsing
(defn string->object
  [s]
  (util/handler-case
   (let [res (ob/nquads-object->object s)]
     (base/remove-falsies
      [[:oi (::rdf/iri res)]
       [:ol (::rdf/lexical res)]
       [:ln (::rdf/language res)]
       [:di (::rdf/datatype res)]]))
   (:default e {:oi s})))

(defn req->query
  [req]
  (merge
   {:per-page base/+per-page+ :page 0}
   (if-let [obj (get-in req [:params "object"])]
     (string->object obj))
   (base/updates
    (set/rename-keys
     (clojure.walk/keywordize-keys
      (select-keys
       (:params req)
       ["graph" "subject" "predicate" "per-page" "page"]))
     {:graph :gi :subject :si :predicate :pi})
    :per-page #(min base/+per-page+ (max 1 (base/str->int % base/+per-page+)))
    :page #(max 0 (base/str->int % 0)))))

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

(defmethod query-stream :default
  [query data]
  (filter (partial matches-query? query) data))

(defmethod query :default ;; default is an in-memory sequence, which we just filter.
  [query data]
  (paginated (get query :per-page base/+per-page+) (get query :page 0)
             (query-stream query data)))
