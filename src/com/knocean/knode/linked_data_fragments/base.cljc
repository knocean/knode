(ns com.knocean.knode.linked-data-fragments.base
  (:require [clojure.edn :as edn]
            [clojure.string :as string]
            [clojure.set :as set]

            [org.knotation.util :as util]
            [org.knotation.rdf :as rdf]))

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
   (try
    (Integer/parseInt str)
    (catch Exception e
      default))))

(defn -data-source-dispatch
  [query data]
  (cond
    (or (vector? data) (list? data)) :default
    (and (map? data) (:connection data)) :database
    (= clojure.lang.Atom (class data)) :atom
    :else :default))

(defmulti query-stream -data-source-dispatch)
(defmulti query -data-source-dispatch)
