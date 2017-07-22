(ns knode.front-end.query-editor
  (:require [clojure.string :as str]
            [crate.core :as crate]

            [knode.front-end.util :as util]
            [knode.front-end.history :as history]
            [knode.front-end.pagination :as pg]))

(def $result (js/$ "#result"))
(def $more-button (js/$ ".more-results"))
(def $send-button (js/$ ".send-query"))

(def editor (.edit js/ace "editor"))
(.setTheme editor "ace/theme/github")
(-> editor (.getSession) (.setMode "ace/mode/sqlserver"))
(.focus editor)

(defn render-error!
  [xhr]
  (-> $result
      (.empty)
      (.append (crate/html [:pre {:class "error"} (.-responseText xhr)]))))

(defn render-result!
  [dat]
  (let [headers (get dat "headers")
        result (get dat "result")
        content [:table {:class "table"}
                 [:tr (for [h headers] [:th h])]
                 (for [row result]
                   [:tr (for [h headers]
                          [:td (interpose
                                " | "
                                (map (fn [val]
                                       (let [v (or (get val "lexical") (get val "curie") (get val "iri"))]
                                         (if-let [href (get val "href")]
                                           [:a {:href href :target "_blank"} v]
                                           v)))
                                     (get row h)))])])]]
    (-> $result
        (.append (crate/html content))
        (util/blink!))))

(defn more!
  []
  (-> (pg/more!)
      (.then (fn [data]
               (let [dat (js->clj (.parse js/JSON data))]
                 (when (empty? (get dat "result")) (.hide $more-button))
                 (render-result! dat))))
      (.fail render-error!)))

(defn new-query!
  [query]
  (history/push! query)
  (pg/new-query! query)
  (-> (pg/more!)
      (.then (fn [data]
               (let [dat (js->clj (.parse js/JSON data))]
                 (.show $more-button)
                 (.empty $result)
                 (render-result! dat))))
      (.fail render-error!)))

(defn add-command! [name keys fn]
  (.addCommand
   editor.commands
   (clj->js {:name name :bindKey keys :exec fn})))

(defn add-commands! [& commands]
  (doseq [[name keys f] commands]
    (add-command! name keys f)))

(add-commands!
 ["sendQuery" {:win "Ctrl-Enter" :mac "Ctrl-Enter"}
  (fn [ed]
    (new-query! (.getValue ed)))]
 ["historyBack" {:win "Ctrl-Up" :mac "Command-Up"}
  (fn [ed]
    (when (= (history/current) (.getValue ed)) (history/prev!))
    (.setValue ed (history/current)))]
 ["historyForward" {:win "Ctrl-Down" :mac "Command-Down"}
  (fn [ed]
    (when (= (history/current) (.getValue ed)) (history/next!))
    (.setValue ed (history/current)))])

(history/get-defaults!)

(.click $send-button #(new-query! (.getValue editor)))
(.click $more-button more!)
(.hide $more-button)
