(ns knode.server.tree-view
  (:require
   [knode.server.template :as pg]))

(defn render-tree-view
  [req name]
  {:status 200
   :headers {"Content-Type" "text/html"}
   :body
   (pg/base-template
    req
    {:title (str name " - Tree View")
     :content [:div
               [:div {:class "tree"}]
               [:link {:href "/assets/inspire-tree/inspire-tree.min.css" :rel "stylesheet"}]
               [:script {:src "/assets/inspire-tree/lodash.min.js" :type "text/javascript" :charset "utf-8"}]
               [:script {:src "/assets/inspire-tree/inspire-tree.min.js" :type "text/javascript" :charset "utf-8"}]
               [:script {:src "/assets/inspire-tree/inspire-tree-dom.min.js" :type "text/javascript" :charset "utf-8"}]
               [:script {:src "/js/tree_view.js" :type "text/javascript" :charset "utf-8"}]]})})
