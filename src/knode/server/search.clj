(ns knode.server.search
  (:require
   [knode.text-search :as search]

   [knode.server.template :as pg]
   [knode.server.util :as util]))

(defn render-search-results [req]
  {:status 200
   :headers {"Content-Type" "application/edn"}
   :body (str ["foo" "bar" "baz"])})

(defn render-search-interface [req]
  {:status 200
   :headers {"Content-Type" "text/html"}
   :body (pg/base-template
          req
          {:title "Search Interface"
           :content [:div [:p "Welcome to the search interface!"]]})})
