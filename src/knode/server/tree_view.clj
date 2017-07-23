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
               [:p "Stubbed tree-view for " [:code name] " goes here."]
               [:div {:class ".tree"}]
               [:script {:src "/assets/lodash.min.js" :type "text/javascript" :charset "utf-8"}] ;; required by inspire-tree
               [:script {:src "/assets/inspire-tree.min.js" :type "text/javascript" :charset "utf-8"}]
               [:script {:src "/js/tree_view.js"}]]})})
