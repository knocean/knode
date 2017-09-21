(ns knode.front-end.text-search.text-search
  (:require [cljs.reader :as reader]

            [crate.core :as crate]

            [knode.front-end.util :as util]))

(defn clear-results!
  []
  (-> (js/$ ".results") (.empty)))

(defn render-results!
  [data]
  (let [dat (reader/read-string data)
        $res (js/$ ".results")]
    (-> $res
        (.empty)
        (.append
         (crate/html
          [:ul {:class "list-group"}
           (for [entry dat]
             (let [href (or (get entry "href") (get entry "iri"))
                   label (get entry "label")
                   alt (get entry "alternative term")]
               [:li {:class "list-group-item"}
                [:a {:href href :target "_blank"}
                 (get entry "label")]
                (when alt [:i " (or " alt ")"])]))])))))

(defn run-search!
  [term]
  (when (not (empty? term))
    (util/$get "/api/search" {:search-term term} render-results!)))

(defn initialize-search-interface!
  [$search-text $search-button $clear-button]
  (let [clear! (fn []
                 (clear-results!)
                 (-> $search-text (.val "")))]
    (.keyup
     $search-text
     #(if (= (.-which %) 27)
        (clear!)
        (run-search! (.val $search-text))))
    (.click $search-button #(run-search! (.val $search-text)))
    (.click $clear-button #(clear!))))

(util/dom-loaded
 #(initialize-search-interface!
   (js/$ ".search-text")
   (js/$ ".search-button")
   (js/$ ".clear-button")))
