(ns knode.front-end.tree-view.tree_view
  (:require
   [cljs.reader :as reader]
   [knode.front-end.util :as util :refer [log! tap!]]))

(defn triples->parent-lookup [triples]
  (reduce
   (fn [memo [subject parent label]]
     (assoc memo parent
            (conj
             (get memo parent [])
             {:text label :iri subject})))
   {} triples))

(defn -tree-from-lookup [table elem]
  (let [children (get table (get elem :iri))]
    (if (empty? children)
      elem
      (assoc elem :children
             (let [final-children (vec (map (partial -tree-from-lookup table)
                                            children))]
               (if (empty? final-children)
                 true
                 final-children))))))

(defn build-tree [triples]
  (let [tbl (triples->parent-lookup triples)]
    (vec (map (partial -tree-from-lookup tbl)
              (concat (get tbl nil)
                      (get tbl ""))))))

(defn tree-name []
  (.-tree-name js/document))

(defn setup-tree-view []
  (let [tree (new js/InspireTree (clj->js {:data (fn [node resolve reject]
                                                   (-> js/$
                                                       (.getJSON
                                                        (str "/api/tree/" (.-tree-name js/document) "/children")
                                                        (clj->js {:root (when node (.-iri node))}))))}))]
    (new js/InspireTreeDOM tree (clj->js {:target ".tree"}))))

(util/dom-loaded setup-tree-view)
