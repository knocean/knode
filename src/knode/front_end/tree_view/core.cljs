(ns knode.front-end.tree-view.core)

(def sample-trips
  [["d" "c" "D"]
   ["b" "a" "B"]
   ["c" "f" "C"]
   ["f" "a" "F"]
   ["c" "b" "C"]
   ["e" "c" "E"]
   ["a" "" "A"]])

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

(def tree (new js/InspireTree (clj->js {:data sample-tree})))
(new js/InspireTreeDOM tree (clj->js {:target ".tree"}))
