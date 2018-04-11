(ns com.knocean.knode.pages.ontology.html
  (:require [clojure.string :as string]
            [clj-jgit.porcelain :as git]

            [org.knotation.rdf :as rdf]
            [org.knotation.rdfa :as rdfa]
            [org.knotation.environment :as en]
            [org.knotation.link :as ln]

            [com.knocean.knode.state :refer [state] :as st]
            [com.knocean.knode.pages.authentication :as auth]
            [com.knocean.knode.pages.html :refer [html]]
            [com.knocean.knode.pages.ontology.base :refer [ontology-result] :as base]
            [com.knocean.knode.pages.ontology.template :as tmp]))

(defn render-html-subject
  [env iri]
  [:div
   [:p [:strong "Subject"] ": " [:a {:href iri} iri]]
   (into
    [:table.table
     {:resource iri}
     [:tr
      [:th "Predicate"]
      [:th "Object"]]
     (->> @state
          :states
          (filter #(= (::rdf/subject %) {::rdf/iri iri}))
          (filter ::rdf/predicate)
          (map #(rdfa/render-pair-row (::en/env %) %))
          (string/join "\n"))]
    (let [objects
          (->> @state
               :states
               (filter #(= (::rdf/object %) {::rdf/iri iri})))
          predicates
          (->> objects
               (map ::rdf/predicate)
               distinct)]
      (for [predicate predicates]
        [:tr
         [:td
          (rdfa/render-link env predicate)
          " THIS"]
         (->> objects
              (filter #(= (::rdf/predicate %) predicate))
              (map (fn [q] [:li (rdfa/render-subject (::en/env q) q)]))
              (into [:ul])
              (conj [:td]))])))
   (let [usage
         (->> @state
              :states
              (filter #(= (::rdf/predicate %) {::rdf/iri iri}))
              (map #(rdfa/render-triple-row (::en/env %) %)))]
     (when (first usage)
       [:div
        [:table.table
         [:tr
          [:th "Subject"]
          [:th "Predicate"]
          [:th "Object"]]
         (string/join "\n" usage)]]))])

(defmethod ontology-result "html"
  [{:keys [requested-iris env params session] :as req}]
  (html
   {:session session
    :title "Ontology Page"
    :content 
    (case (count requested-iris)
      1 (render-html-subject env (first requested-iris))
      0 [:div
         (when (auth/logged-in? req)
           (list [:div {:class "col-md-6"}
                  [:h3 "Add Term"]
                  [:script {:type "text/javascript" :src "/js/knode.js"}]
                  [:form {:method "POST" :action "/ontology/validate-term"}
                   [:textarea {:id "editor" :rows "6" :name "template-text"}
                    (tmp/template-dummy (tmp/template-by-iri (first (tmp/list-templates))))]
                   [:input {:class "btn btn-primary" :type "submit" :value "Validate Term"}]]]
                 [:div {:class "col-md-6"}
                  [:p "Templates " [:i "(required predicates)"] ":"]
                  [:ul
                   (map
                    (fn [iri]
                      (let [t (tmp/template-by-iri iri)]
                        [:li (:name t) [:i " (" (string/join ", " (:predicates t)) ")"]]))
                    (tmp/list-templates))]]))
         [:div {:class "col-md-12"}
          [:h3 "Term list"]
          [:ul (map (fn [s] [:li (rdfa/render-link env s)])
                    (base/all-subjects))]]]
      (map #(render-html-subject env) requested-iris))}))
