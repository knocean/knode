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
               {:keys [internal-meta final-iri] :as meta} (up/get-upstream-meta! iri)
               {:keys [title name]} internal-meta
               [deleted added] (up/upstream-delta! iri)]
           (pg/base-template
            req
            {:title (str (or title name) " - Delta Report")
             :content
             [:div
              [:h1 (or title name) " Delta"]
              [:p (str meta)]
              (when (not= iri final-iri)
                [:i [:code "[" iri " -> " final-iri "]"]])
              (when (and (empty? deleted) (empty? added))
                [:p "No terms changed..."])
              (when (not (empty? deleted))
                [:div
                 [:h3 "Removed Terms:"]
                 [:ul (map (fn [term] [:li term]) deleted)]])
              (when (not (empty? added))
                [:div
                 [:h3 "New Terms:"]
                 [:ul (map (fn [term] [:li term]) added)]])]}))})

(defn render-upstream-report
  [req]
  {:status 200
   :headers {"Content-Type" "text/html"}
   :body (pg/base-template
          req
          {:title "Upstream Ontology Report"
           :content [:ul (map (fn [{:keys [iri final-iri version-iri bytes title name]}]
                                [:li
                                 (or title name) " - " [:a {:href final-iri :hover final-iri} "Source"]
                                 (when (not= iri final-iri)
                                   [:code "[" iri " -> " final-iri "]"])
                                 (when (not (util/login? req))
                                   [:form
                                    {:method "GET" :action "/dev/upstream/delta"}
                                    [:input {:type "hidden" :name "ontology" :value iri}]
                                    [:input {:type "submit" :value "Refresh"}]])])
                              (up/upstream-report!))]})})
