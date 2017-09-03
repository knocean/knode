(ns knode.front-end.text-search.text-search
  (:require [cljs.reader :as reader]

            [crate.core :as crate]

            [knode.front-end.util :as util]))

(defn render-results!
  [data]
  (let [dat (reader/read-string data)
        $res (js/$ ".results")]
    (util/log! "DATA:" dat)
    (-> $res
        (.empty)
        (.append
         (crate/html
          [:ul {:class "list-group"}
           (for [entry dat]
             (let [href (or (get entry "href") (get entry "iri"))
                   label (get entry "label")
                   alt (get entry "alternative term")]
               (util/log! href label alt)
               [:li {:class "list-group-item"}
                [:a {:href href :target "_blank"}
                 (get entry "label")]
                (when alt [:i " (or " alt ")"])]))])))))

(defn run-search!
  [term]
  (util/$get "/api/search" {:search-term term} render-results!))

(defn initialize-search-interface!
  [$search-text $search-button]

  (.keyup
   $search-text
   #(when (= 13 (.-which %))
      (run-search! (.val $search-text))))
  (.click
   $search-button
   #(run-search! (.val $search-text))))

(util/dom-loaded
 #(initialize-search-interface! (js/$ ".search-text") (js/$ ".search-button")))
