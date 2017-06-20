(ns knode.front-end.history
  (:require [cljs.reader :as reader]))

(def storage? (not (not (type js/Storage))))

(def history (atom (reader/read-string (or (and storage? (.getItem js/localStorage "query-history")) "[]"))))
(defn history-max [] (max 0 (- (count @history) 1)))
(def history-position (atom (history-max)))

(defn max? [] (= (history-max) @history-position))

(defn push! [query]
  (when (and (not (empty? query)) (not (= (last @history) query)))
    (when (= @history-position (history-max))
      (swap! history-position inc))
    (swap! history conj query)
    (when storage?
      (.setItem js/localStorage "query-history" @history))))

(defn next! []
  (swap! history-position (fn [ct] (min (history-max) (inc ct)))))
(defn prev! []
  (swap! history-position (fn [ct] (max 0 (dec ct)))))
(defn current []
  (get @history @history-position))
