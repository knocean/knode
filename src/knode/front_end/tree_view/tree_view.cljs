(ns knode.front-end.tree-view.tree_view
  (:require [knode.front-end.util :as util :refer [log! tap!]]))

(def sample-trips
  [["d" "c" "D"]
   ["b" "a" "B"]
   ["c" "f" "C"]
   ["f" "a" "F"]
   ["c" "b" "C"]
   ["e" "c" "E"]
   ["a" nil "A"]])

(def sample-tree
  [{:text "A" :iri "a"
    :children
    [{:text "B" :iri "b"
      :children
      [{:text "C" :iri "c"
        :children
        [{:text "D" :iri "d"}
         {:text "E" :iri "e"}]}]}
     {:text "F" :iri "f"
      :children
      [{:text "C" :iri "c"
        :children
        [{:text "D" :iri "d"}
         {:text "E" :iri "e"}]}]}]}])

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
             (vec (map (partial -tree-from-lookup table)
                       children))))))

(defn build-tree [triples]
  (let [tbl (triples->parent-lookup triples)]
    (vec (map (partial -tree-from-lookup tbl)
              (concat (get tbl nil)
                      (get tbl ""))))))

(defn setup-tree-view []
  (.log js/console (str "TREE NAME:" (.-tree-name js/document)))
  (-> js/$
      (.get "/api/tree/foo/nodes" (clj->js {}))
      (.done (fn [data] (.log js/console data))))
  (let [tree (new js/InspireTree (clj->js {:data (build-tree sample-trips)}))]
    (new js/InspireTreeDOM tree (clj->js {:target ".tree"}))))

(util/dom-loaded setup-tree-view)
