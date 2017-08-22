(ns knode.text-search
  (:require [clucy.core :as clucy]))

(def +search-results+ 25)
(def index (clucy/disk-index "./search-index"))

(defn add! [map]
  (clucy/add index map))

(defn search [term]
  (clucy/search index term +search-results+))

(defn complete [term] [])
