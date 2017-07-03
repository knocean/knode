(ns knode.server.query
  (:require
   [clojure.string :as string]
   [knode.server.template :as pg]
   [knode.server.util :as util]))

(def +default-limit+ 200)

(defn -ensure-limit [query]
  (if-let [res (re-find #"LIMIT (\d+)" query)]
    (let [given-limit (read-string (second res))]
      (if (> given-limit +default-limit+)
        [(string/replace query (re-pattern (first res)) (str "LIMIT " +default-limit+)) +default-limit+]
        [query given-limit]))
    [(str query "\nLIMIT " +default-limit+) +default-limit+]))

(defn -ensure-offset [query limit page]
  (if (or (= 0 page) (re-find #"OFFSET (\d+)" query))
    query
    (str query "\nOFFSET " (* limit page))))

(defn sanitized-sparql
  [query & {:keys [page] :or {page 0}}]
  (let [[query limit] (-ensure-limit query)]
    (-ensure-offset query limit page)))

(defn render-query-interface
  [req]
  {:status 200
   :headers {"Content-Type" "text/html"}
   :body (pg/base-template
          req
          {:title "Query Interface"
           :content [:div
                     [:div {:id "editor"}]
                     [:button {:class "send-query"} "Query"]
                     [:div {:id "result"}]
                     [:script {:src "/assets/ace/ace.js" :type "text/javascript" :charset "utf-8"}]]})})
