(ns knode.server.upstream
  (:require
   [knode.upstream :as up]

   [knode.server.template :as pg]
   [knode.server.util :as util]))

(defn render-upstream-delta
  [req]
  {:status 200
   :headers {"Content-Type" "text/html"}
   :body (let [iri (get-in req [:params "ontology"])
               meta (up/get-upstream-meta! iri)]
           (pg/base-template
            req
            {:title (str (:name meta) " - Delta Report")
             :content
             [:div
              [:p (str "Upstream: " iri)]
              [:ul [:li "Removed Terms"]]
              [:ul [:li "New Terms"]]]}))})

(defn render-upstream-report
  [req]
  {:status 200
   :headers {"Content-Type" "text/html"}
   :body (pg/base-template
          req
          {:title "Upstream Ontology Report"
           :content [:ul (map (fn [{:keys [iri final-iri version-iri bytes name]}]
                                [:li
                                 [:a {:href ""} name] " - " [:a {:href final-iri} "Source"] " - " iri
                                 (when (not (util/login? req))
                                   [:form
                                    {:method "GET" :action "/dev/upstream/delta"}
                                    [:input {:type "hidden" :name "ontology" :value iri}]
                                    [:input {:type "submit" :value "Refresh"}]])])
                              (up/upstream-report!))]})})
