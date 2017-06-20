(ns knode.front-end.core
  (:require [cljs.reader :as reader]))

(def editor (.edit js/ace "editor"))
(.setTheme editor "ace/theme/monokai")
(-> editor (.getSession) (.setMode "ace/mode/sqlserver"))

(def storage? (not (not (type js/Storage))))

(def history (atom (reader/read-string (or (and storage? (.getItem js/localStorage "query-history")) "[]"))))
(defn history-max [] (max 0 (- (count @history) 1)))
(def history-position (atom (history-max)))

(defn history-push! [query]
  (when (and (not (empty? query)) (not (= (last @history) query)))
    (when (= @history-position (history-max))
      (swap! history-position inc))
    (swap! history conj query)
    (when storage?
      (.setItem js/localStorage "query-history" @history))))

(defn history-next! []
  (swap! history-position (fn [ct] (min (history-max) (inc ct)))))
(defn history-prev! []
  (swap! history-position (fn [ct] (max 0 (dec ct)))))
(defn history-current []
  (get @history @history-position))

(defn send-query [query]
  (.log js/console "Sending current query...")
  (.log js/console query)
  (history-push! query)
  (-> js/$
      (.get "/api/query" (clj->js {:sparql query}))
      (.done (fn [data]
               (.log js/console "GOT RESPONSE")
               (.log js/console data)))))

(defn add-command [name keys fn]
  (.addCommand
   editor.commands
   (clj->js {:name name :bindKey keys :exec fn})))

(add-command
 "sendQuery"
 {:win "Ctrl-Enter" :mac "Ctrl-Enter"}
 (fn [ed]
   (send-query (.getValue ed))))

(add-command
 "historyBack"
 {:win "Ctrl-Up" :mac "Command-Up"}
 (fn [ed]
   (when (= (history-current) (.getValue ed)) (history-prev!))
   (.setValue ed (history-current))
   (.log js/console "Previous history query...")))

(add-command
 "historyForward"
 {:win "Ctrl-Down" :mac "Command-Down"}
 (fn [ed]
   (when (= (history-current) (.getValue ed)) (history-next!))
   (.setValue ed (history-current))
   (.log js/console "Next history query...")))

(-> (js/$ ".send-query")
    (.click #(send-query (.getValue editor))))
