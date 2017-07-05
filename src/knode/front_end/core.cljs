(ns knode.front-end.core
  (:require [clojure.string :as str]
            [crate.core :as crate]

            [knode.front-end.history :as history]
            [knode.front-end.query :as query]))

(def $result (js/$ "#result"))
(def $more-button (js/$ ".more-results"))
(def $send-button (js/$ ".send-query"))

(def editor (.edit js/ace "editor"))
(.setTheme editor "ace/theme/monokai")
(-> editor (.getSession) (.setMode "ace/mode/sqlserver"))

(defn blink! [selector & {:keys [delay color] :or {delay 500 color "#FFFF9C"}}]
  (let [$el (js/$ selector)
        origin (or (-> $el (.css "background-color")) "initial")]
    (-> $el
        (.stop) (.css "background-color" color)
        (.fadeTo 100 0.3 (fn [] (-> $el (.fadeTo delay 1.0 (fn [] (-> $el (.css "background-color" origin))))))))))

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
                          [:td (str/join " | "
                                         (map (fn [val] val
                                                (or (get val "lexical") (get val "curie") (get val "iri")))
                                              (get row h)))])])]]
    (.log js/console "RENDER-RESULT!" dat)
    (.log js/console " ===== " (get dat "result") (crate/html content))
    (-> $result
        (.append (crate/html content))
        (blink!))))

(defn more!
  []
  (-> (query/more!)
      (.then (fn [data]
               (let [dat (js->clj (.parse js/JSON data))]
                 (.log js/console "RESULT" dat)
                 ;; (when (empty? (get dat "result")) (.hide $more-button))
                 (render-result! dat))))
      (.fail render-error!)))

(defn new-query!
  [query]
  (history/push! query)
  (query/new-query! query)
  (-> (query/more!)
      (.then (fn [data]
               (let [dat (js->clj (.parse js/JSON data))]
                 (.show $more-button)
                 (.empty $result)
                 (render-result! dat))))
      (.fail render-error!)))

(defn add-command [name keys fn]
  (.addCommand
   editor.commands
   (clj->js {:name name :bindKey keys :exec fn})))

(add-command
 "sendQuery"
 {:win "Ctrl-Enter" :mac "Ctrl-Enter"}
 (fn [ed]
   (new-query! (.getValue ed))))

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

(.click $send-button #(new-query! (.getValue editor)))
(.click $more-button more!)
(.hide $more-button)
