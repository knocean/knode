(ns knode.server.upstream
  (:require
   [knode.upstream :as up]

   [knode.server.template :as pg]))

(defn render-upstream-delta
  [upstream req]
  {:status 200
   :headers {"Content-Type" "text/html"}
   :body (let [iri (get-in req [:params "ontology"])
               upstream-name "Foo"]
           (pg/base-template
            req
            {:title (str upstream-name " - Delta Report")
             :content
             [:div
              [:ul [:li "Removed Terms"]]
              [:ul [:li "New Terms"]]]}))})

(defn render-upstream-report
  [req]
  {:status 200
   :headers {"Content-Type" "text/html"}
   :body (pg/base-template
          req
          {:title "Upstream Ontology Report"
           :content [:ul (map (fn [{:keys [final-iri version-iri bytes name]}]
                                [:li
                                 [:a {:href "#"} name] " - " [:a {:href final-iri} "Source"]
                                 [:form
                                  {:method "POST"}
                                  [:input {:type "hidden" :name "ontology" :value final-iri}]
                                  [:input {:type "submit" :value "Refresh"}]]])
                              (up/upstream-report!))]})})
