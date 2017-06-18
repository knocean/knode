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
                     [:div {:id "editor" :style "width: 100%; height: 400px;"}]
                     [:button {:class "send-query"} "Query"]
                     [:script {:src "/assets/ace/ace.js" :type "text/javascript" :charset "utf-8"}]
                     [:script
                      "var editor = ace.edit(\"editor\"); editor.setTheme(\"ace/theme/monokai\"); editor.getSession().setMode(\"ace/mode/sqlserver\");"]]})})
