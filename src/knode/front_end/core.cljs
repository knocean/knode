(ns knode.front-end.core
  (:require [knode.front-end.history :as history]))

(def editor (.edit js/ace "editor"))
(.setTheme editor "ace/theme/monokai")
(-> editor (.getSession) (.setMode "ace/mode/sqlserver"))

(defn send-query [query]
  (.log js/console "Sending current query...")
  (.log js/console query)
  (history/push! query)
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
   (when (= (history/current) (.getValue ed)) (history/prev!))
   (.setValue ed (history/current))))

(add-command
 "historyForward"
 {:win "Ctrl-Down" :mac "Command-Down"}
 (fn [ed]
   (when (= (history/current) (.getValue ed)) (history/next!))
   (.setValue ed (history/current))))

(-> (js/$ ".send-query")
    (.click #(send-query (.getValue editor))))
