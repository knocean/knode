(ns knode.front-end.core)

(def editor (.edit js/ace "editor"))
(.setTheme editor "ace/theme/monokai")
(-> editor (.getSession) (.setMode "ace/mode/sqlserver"))

(-> (js/$ ".send-query")
    (.click
     (fn []
       (let [query (.getValue editor)]
         ;; (-> editor)
         (.log js/console query)
         (-> js/$
             (.get "/query" (clj->js {:sparql query}))
             (.done (fn [data]
                      (.log js/console "GOT RESPONSE")
                      (.log js/console data))))))))
