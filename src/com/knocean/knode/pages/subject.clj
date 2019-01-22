(ns com.knocean.knode.pages.subject
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [cheshire.core :as json]

            [org.knotation.rdf :as rdf]
            [org.knotation.environment :as en]
            [com.knocean.knode.util :as util]
            [com.knocean.knode.state :refer [state] :as st]
            [com.knocean.knode.pages.html :refer [html]]))

(defn render-link
  [env iri]
  [:a {:href (str "/subject?iri=" (util/escape iri))} (en/iri->name env iri)])

(defn render-object
  [env {:keys [oi ob ol dt ln] :as state}]
  (cond
    oi (render-link env oi)
    ob ob
    ol ol))

(defn render-pair-row
  [env {:keys [pi] :as state}]
  [:tr
   [:td (render-link env pi)]
   [:td (render-object env state)]])

(defn render-triple-row
  [env {:keys [si sb pi] :as state}]
  [:tr
   [:td (if si (render-link env si) sb)]
   [:td (render-link env pi)]
   [:td (render-object env state)]])

(defn subject-by-iri
  [iri]
  (st/select [:= :si iri]))

(defn subject
  [{:keys [query-params] :as req}]
  {:status 200
   :headers {"Content-Type" "application/json"}
   :body (json/encode (subject-by-iri (get query-params "iri")))})

(defn render-subject-html
  [iri]
  (let [states (st/select [:= :si iri])]
    [:div
     [:p [:strong "Subject"] ": " [:a {:href iri} iri]]
     [:div
      "Download as "
      [:a {:href (str "/subject/?iri=" (util/escape iri) "&format=ttl")} "Turtle (ttl)"]
      ", JSON, TSV."]
     (into
      [:table.table
       {:resource iri}
       [:tr
        [:th "Predicate"]
        [:th "Object"]]]
      (concat
       (->> states
            (filter #(= (:si %) iri))
            (filter :pi)
            (map #(render-pair-row (::en/env %) %)))))]))
       ;(let [env (st/latest-env)
       ;      objects
       ;      (->> states
       ;           (filter #(= (:oi %) iri)))
       ;      predicates
       ;      (->> objects
       ;           (map :pi)
       ;           distinct))
       ;  (for [predicate predicates]
       ;    [:tr
       ;     [:td
       ;      (render-link env predicate)
       ;      " THIS"]
       ;     (->> objects
       ;          (filter #(= (:pi %) predicate))
       ;          (map (fn [q] [:li (render-link (::en/env q) (:si q))]))
       ;          (into [:ul])
       ;          (conj [:td]))]))))])
     ;(let [env (st/latest-env)
     ;      usage
     ;      (->> states
     ;           (filter #(= (:pi %) iri))
     ;           (map #(render-triple-row env %)))))
     ;  (when (first usage)
     ;    [:div
     ;     [:h3 "Usage"]
     ;     (into
     ;      [:table.table
     ;       [:tr
     ;        [:th "Subject"]
     ;        [:th "Predicate"]
     ;        [:th "Object"]]]
     ;      usage)])))])

(defn render-subject
  [iri]
  {:title "Subject"
   :content (render-subject-html iri)})

(defn subject-page
  [{:keys [query-params session] :as req}]
  (let [iri (get query-params "iri")]
    (html (assoc (render-subject iri) :session session))))

(def routes
  [["/subject" subject-page]
   ["/api/subject" subject]])
