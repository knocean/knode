(ns knode.server.search
  (:require
   [knode.text-search :as search]
   [knode.state :refer [state]]

   [knode.server.template :as pg]
   [knode.server.util :as sutil]))

(defn render-search-results [req]
  {:status 200
   :headers {"Content-Type" "application/edn"}
   :body (str
          (vec
           (map
            #(assoc % "href" (sutil/re-root @state req (get % "iri")))
            (search/search (get-in req [:params "search-term"] "")))))})

(defn render-search-interface [req]
  {:status 200
   :headers {"Content-Type" "text/html"}
   :body (pg/base-template
          req
          {:title "Search Interface"
           :content [:div
                     [:div {:class "input-group"}
                      [:input {:type "text" :placeholder "Search" :class "form-control search-text"
                               :autocomplete "off" :autocorrect "off" :autocapitalize "off" :spellcheck "false"}]
                      [:span {:class "input-group-btn"}
                       [:input {:type "button" :class "search-button btn btn-primary" :value "Search"}]
                       [:input {:type "button" :class "clear-button btn btn-primary" :value "Clear"}]]]
                     [:div {:class "results"}]
                     [:script {:src "/js/text_search.js" :type "text/javascript" :charset "utf-8"}]]})})
