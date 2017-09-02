(ns knode.server.search
  (:require
   [knode.text-search :as search]

   [knode.server.template :as pg]
   [knode.server.util :as util]))

(defn render-search-results [req]
  {:status 200
   :headers {"Content-Type" "application/edn"}
   :body (str (vec (search/search (get-in req [:params "search-term"] ""))))})

(defn render-search-interface [req]
  {:status 200
   :headers {"Content-Type" "text/html"}
   :body (pg/base-template
          req
          {:title "Search Interface"
           :content [:div
                     [:div {:class "input-group"}
                      [:input {:type "text" :placeholder "Search" :class "form-control search-text"}]
                      [:span {:class "input-group-btn"}
                       [:input {:type "button" :class "search-button btn btn-primary" :value "Search"}]]]
                     [:div {:class "results"}]
                     [:script {:src "/js/text_search.js" :type "text/javascript" :charset "utf-8"}]]})})
