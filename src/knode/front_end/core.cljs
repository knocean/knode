(ns knode.front-end.core
  (:require [clojure.string :as str]
            [crate.core :as crate]

            [knode.front-end.history :as history]))

(def editor (.edit js/ace "editor"))
(.setTheme editor "ace/theme/monokai")
(-> editor (.getSession) (.setMode "ace/mode/sqlserver"))

(defn blink! [selector & {:keys [delay color] :or {delay 500 color "#FFFF9C"}}]
  (let [$el (js/$ selector)
        origin (or (-> $el (.css "background-color")) "initial")]
    (-> $el
        (.stop) (.css "background-color" color)
        (.fadeTo 100 0.3 (fn [] (-> $el (.fadeTo delay 1.0 (fn [] (-> $el (.css "background-color" origin))))))))))

(defn update-result!
  [content]
  (-> (js/$ "#result")
      (.empty)
      (.append (crate/html content)))
  (blink! "#result"))

(defn send-query [query]
  (history/push! query)
  (-> js/$
      (.get "/api/query" (clj->js {:sparql query :output-format "json"}))
      (.done (fn [data]
               (let [dat (js->clj (.parse js/JSON data))
                     headers (get dat "headers")
                     result (get dat "result")]
                 (update-result!
                  [:table {:class "table"}
                   [:tr (for [h headers] [:th h])]
                   (for [row result]
                     [:tr (for [h headers]
                            [:td (str/join " | "
                                           (map (fn [val] val
                                                  (or (get val "lexical") (get val "curie") (get val "iri")))
                                                (get row h)))])])]))))
      (.fail (fn [xhr] (update-result! [:pre {:class "error"} (.-responseText xhr)])))))

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

(history/get-defaults!)

(-> (js/$ ".send-query")
    (.click #(send-query (.getValue editor))))
