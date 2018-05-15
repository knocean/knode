(ns com.knocean.knode.pages.ontology.html
  (:require [clojure.string :as string]
            [clj-jgit.porcelain :as git]

            [org.knotation.rdf :as rdf]
            [org.knotation.environment :as en]
            [org.knotation.link :as ln]

            [com.knocean.knode.state :refer [state] :as st]
            [com.knocean.knode.pages.authentication :as auth]
            [com.knocean.knode.pages.html :refer [html]]
            [com.knocean.knode.pages.ontology.base :refer [ontology-result] :as base]
            [com.knocean.knode.pages.ontology.template :as tmp]))

(defn escape
  [iri]
  (java.net.URLEncoder/encode iri))

(defn render-link
  [env iri]
  [:a {:href iri} (ln/iri->name env iri)])

(defn local-link
  [env iri]
  (let [curie (ln/iri->curie env iri)]
    (str "/ontology/" (string/replace curie ":" "_"))))

(defn render-subject
  [env iri]
  (let [curie (ln/iri->curie env iri)]
    [:a
     {:href (str "/ontology/" (string/replace curie ":" "_"))}
     curie
     " "
     (ln/iri->name env iri)]))

(defn render-pair
  [env {:keys [pi oi ob ol] :as state}]
  [:li
   (render-link env pi)
   ": "
   (cond
     oi [:a {:href oi :property (ln/iri->curie env pi)} (ln/iri->name env oi)]
     ob ob
     ol [:span {:property (ln/iri->curie env pi)} ol])])

(def obo (partial apply str "http://purl.obolibrary.org/obo/"))

(def predicates
  [(rdf/rdf "type")
   (rdf/rdfs "label")
   (obo "IAO_0000118")
   (rdf/rdfs "subClassOf")
   (obo "ncbitaxon#has_rank")])

(defn render-subject-html
  [iri]
  (let [env (st/latest-env)
        states (st/select (format "si='%s'" iri))]
    [:div
     [:h2 (ln/iri->curie env iri) " " (ln/iri->name env iri)]
     [:p [:a {:href iri} iri]]
     (->> states
          (map :pi)
          (remove nil?)
          distinct
          (remove (set predicates))
          (into predicates)
          (mapcat
           (fn [predicate]
             (->> states
                  (filter #(= predicate (:pi %)))
                  (map (partial render-pair env)))))
          (into [:ul]))
     [:div
      "Other formats: "
      [:a {:href (str (local-link env iri) ".ttl")} "Turtle (ttl)"]
      ", "
      [:a {:href (str (local-link env iri) ".json")} "JSON-LD (json)"]
      ", "
      [:a {:href (str (local-link env iri) ".tsv")} "TSV (tsv)"]
      "."]]))

     ;(into
     ; [:table.table
     ;  {:resource iri}
     ;  [:tr
     ;   [:th "Predicate"]
     ;   [:th "Object"]]]
     ; (concat
     ;  (->> states
     ;       (filter :pi)
     ;       (map #(render-pair-row (::en/env %) %))))]]))

(defmethod ontology-result "html"
  [{:keys [requested-iris env params session] :as req}]
  (html
   (case (count requested-iris)
     0 {:session session
        :title (:idspace @state)
        :content
        [:div
           ;(when (auth/logged-in? req)
           ;  (list [:div {:class "col-md-6"}
           ;         [:h3 "Add Term"]
           ;         [:script {:type "text/javascript" :src "/js/knode.js"}]
           ;         [:form {:method "POST" :action "/ontology/validate-term"}
           ;          [:textarea {:id "editor" :rows "6" :name "template-text"}
           ;           (tmp/template-dummy (tmp/template-by-iri (first (tmp/list-templates))))]
           ;          [:input {:class "btn btn-primary" :type "submit" :value "Validate Term"}]]]
           ;        [:div {:class "col-md-6"}
           ;         [:p "Templates " [:i "(required predicates)"] ":"]
           ;         [:ul
           ;          (map
           ;           (fn [iri]
           ;             (let [t (tmp/template-by-iri iri)]
           ;               [:li (:name t) [:i " (" (string/join ", " (:predicates t)) ")"]]))
           ;           (tmp/list-templates))]])
         [:h3 "Term List"]
         [:p
          "Other formats: "
          [:a {:href (str "/ontology/" (:idspace @state) ".ttl")} "Turtle (ttl)"]
          ", "
          [:a {:href (str "/ontology/" (:idspace @state) ".tsv")} "TSV (tsv)"]
          "."]
         [:ul (map (fn [s] [:li (render-subject env s)])
                   (base/all-subjects))]]}
     1 {:session session
        :title (ln/iri->name env (first requested-iris))
        :content (render-subject-html (first requested-iris))}
     {:session session
      :title (:idspace @state)
      :content (map render-subject-html requested-iris)})))
