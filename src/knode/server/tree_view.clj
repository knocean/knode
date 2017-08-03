(ns knode.server.tree-view
  (:require
   [clojure.java.io :as io]
   [clojure.edn :as edn]
   [clojure.data.json :as json]

   [knode.state :refer [state]]
   [knode.server.template :as pg]))

(defn read-tree! [tree-name]
  (try
    (let [fname (str (:ontology-dir @state) "trees.edn")]
      (get (edn/read (java.io.PushbackReader. (io/reader fname))) tree-name []))
    (catch Exception e
      [])))

(defn render-tree-data
  [req name]
  {:status 200
   :headers {"Content-Type" "application/edn"}
   :body (str (read-tree! name))})

(defn render-tree-children
  [req name]
  {:status 200
   :headers {"Content-Type" "application/json"}
   :body (let [tree (read-tree! name)
               root (get-in req [:params "root"])]
           (json/write-str
            (vec (map (fn [[subject parent label]]
                        {:text label :iri subject :id subject
                         :children (some #(= subject (second %)) tree)})
                      (filter
                       (if (empty? root)
                         (fn [[a b c]] (nil? b))
                         (fn [[a b c]] (= b root)))
                       tree)))))})

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
               [:script {:src "/js/tree_view.js" :type "text/javascript" :charset "utf-8"}]
               [:script {:type "text/javascript" :charset "utf-8"}
                (format "document.tree_name = \"%s\"" name)]]})})
