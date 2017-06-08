(ns knode.server.upstream
  (:require
   [knode.upstream :as up]

   [knode.server.template :as pg]
   [knode.server.util :as util]))

(defn action-form [uri submit & {:keys [method hidden]
                          :or {method "GET"
                               submit "Submit"
                               hidden {}}}]
  [:form {:method method :action uri}
   (map (fn [[k v]] [:input {:type "hidden" :name k :value v}]) hidden)
   [:input {:type "submit" :value submit}]])

(defn replace-upstream!
  [req]
  (let [iri (get-in req [:params "ontology"])
        {:keys [internal-meta final-iri] :as meta} (up/get-upstream-meta! iri)
        {:keys [title name]} internal-meta]
    (up/replace-with-compared! iri)
    {:status 200
     :headers {"Content-Type" "text/html"}
     :body (pg/base-template
            req
            {:title (str (or title name) " - Updated")
             :content
             [:div
              [:h1 (or title name) " Updated"]
              [:p "The ontology " (or title name) " has been updated."]
              [:p [:a {:href "/dev/upstream"} "Click here"] " to return to the upstream report."]]})}))

(defn render-upstream-delta
  [req]
  {:status 200
   :headers {"Content-Type" "text/html"}
   :body (let [iri (get-in req [:params "ontology"])
               {:keys [internal-meta final-iri] :as meta} (up/get-upstream-meta! iri)
               {:keys [title name]} internal-meta
               [deleted added] (up/upstream-delta! iri :fresh-compare? true)]
           (pg/base-template
            req
            {:title (str (or title name) " - Delta Report")
             :content
             [:div
              [:h1 (or title name) " Delta"]
              (when (not= iri final-iri)
                [:p [:i [:code "[" iri " -> " final-iri "]"]]])
              (if (and (empty? deleted) (empty? added))
                [:p "No change since last sync."]
                (action-form
                 "/dev/upstream/delta" "Accept New Version"
                 :method "POST"
                 :hidden {"ontology" iri}))
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
                                   (action-form
                                    "/dev/upstream/delta" "Refresh"
                                    :hidden {"ontology" iri}))])
                              (up/upstream-report!))]})})
