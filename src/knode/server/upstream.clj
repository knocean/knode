(ns knode.server.upstream
  (:require
   [knode.upstream :as up]

   [knode.server.template :as pg]))

(defn render-upstream-report
  [req]
  {:status 200
   :headers {"Content-Type" "text/html"}
   :body (pg/base-template
          req
          {:title "Upstream Ontology Report"
           :content [:ul (map (fn [{:keys [final-iri version-iri bytes name]}]
                                [:li [:a {:href "#"} name] " - " [:a {:href final-iri} "RAW"]])
                              (up/upstream-report))]})})
