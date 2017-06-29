(ns knode.server.query
  (:require
   [knode.server.template :as pg]
   [knode.server.util :as util]))

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
