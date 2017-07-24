(ns knode.front-end.query-editor.query-editor
  (:require [clojure.string :as str]
            [crate.core :as crate]

            [knode.front-end.util :as util]
            [knode.front-end.query-editor.history :as history]
            [knode.front-end.query-editor.pagination :as pg]))

(defn render-error!
     [target-div xhr]
     (-> target-div
         (.empty)
         (.append (crate/html [:pre {:class "error"} (.-responseText xhr)]))))

(defn render-result!
     [target-div dat]
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
       (-> target-div
           (.append (crate/html content))
           (util/blink!))))

(defn more!
  [result-div more-button]
  (-> (pg/more!)
      (.then (fn [data]
               (let [dat (js->clj (.parse js/JSON data))]
                 (when (empty? (get dat "result")) (.hide more-button))
                 (render-result! result-div dat))))
      (.fail (partial render-error! result-div))))

(defn new-query!
  [result-div more-button query]
  (history/push! query)
  (pg/new-query! query)
  (-> (pg/more!)
      (.then (fn [data]
               (let [dat (js->clj (.parse js/JSON data))]
                 (.show more-button)
                 (.empty result-div)
                 (render-result! result-div dat))))
      (.fail (partial render-error! result-div))))

(defn add-command! [ed name keys fn]
  (.addCommand
   (.-commands ed)
   (clj->js {:name name :bindKey keys :exec fn})))

(defn add-commands! [ed & commands]
  (doseq [[name keys f] commands]
    (add-command! ed name keys f)))

(defn initialize-query-editor! [editor-id $result $more-button $send-button]
  (let [editor (.edit js/ace editor-id)]
    (.setTheme editor "ace/theme/github")
    (-> editor (.getSession) (.setMode "ace/mode/sqlserver"))

    (add-commands!
     editor
     ["sendQuery" {:win "Ctrl-Enter" :mac "Ctrl-Enter"}
      (fn [ed]
        (new-query! $result $more-button (.getValue ed)))]
     ["historyBack" {:win "Ctrl-Up" :mac "Command-Up"}
      (fn [ed]
        (when (= (history/current) (.getValue ed)) (history/prev!))
        (.setValue ed (history/current)))]
     ["historyForward" {:win "Ctrl-Down" :mac "Command-Down"}
      (fn [ed]
        (when (= (history/current) (.getValue ed)) (history/next!))
        (.setValue ed (history/current)))])

    (history/get-defaults!)

    (.click $send-button #(new-query! $result $more-button (.getValue editor)))
    (.click $more-button (fn [] (more! $result $more-button)))
    (.hide $more-button)
    (.focus editor)))

(set! (.-onload js/window)
      #(initialize-query-editor!
        "editor" (js/$ "#result")
        (js/$ ".more-results") (js/$ ".send-query")))
