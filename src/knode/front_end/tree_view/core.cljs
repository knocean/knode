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

(.log js/console "HELLO FROM TREE VIEW!")

(def tree (new js/InspireTree (clj->js {:data (fn [] sample-tree)})))
(.log js/console "INSTANTIATED InspireTree")

(new js/InspireTreeDOM tree (clj->js {:target ".tree"}))
(.log js/console "STARTED DOM")
