(ns com.knocean.knode.pages.subject
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [bidi.ring :as bring]
            [cheshire.core :as json]

            [org.knotation.api :as kn]
            [org.knotation.rdf :as rdf]
            [org.knotation.rdfa :as rdfa]
            [org.knotation.environment :as en]
            [com.knocean.knode.state :refer [state] :as st]
            [com.knocean.knode.pages.html :refer [html]]))

(defn subject-by-iri
  [iri]
  (let [env (st/latest-env)
        relevant (->> @state :states
                      (filter #(= (get-in % [::rdf/subject ::rdf/iri]) iri)))]
    (->> relevant
         (filter #(= :org.knotation.state/statement (:org.knotation.state/event %)))
         (map (fn [statement] [(::rdf/predicate statement) (::rdf/object statement)])))))

(defn subject
  [{:keys [query-params] :as req}]
  {:status 200
   :headers {"Content-Type" "application/json"}
   :body (json/encode (subject-by-iri (get query-params "iri")))})

(defn render-subject
  [iri]
  {:title "Subject"
   :content
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
     (let [env (st/latest-env)
           objects
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
          (string/join "\n" usage)]]))]})

(defn subject-page
  [{:keys [query-params session] :as req}]
  (let [iri (get query-params "iri")]
    (html (assoc (render-subject iri) :session session))))

(def routes
  [["/subject" subject-page]
   ["/api/subject" subject]])
