(ns com.knocean.knode.pages.ontology.html
  (:require [clojure.string :as string]
            [clj-jgit.porcelain :as git]

            [org.knotation.rdf :as rdf]
            [org.knotation.environment :as en]
            [org.knotation.omn :as omn]

            [com.knocean.knode.util :as util]
            [com.knocean.knode.state :refer [state] :as st]
            [com.knocean.knode.pages.authentication :as auth]
            [com.knocean.knode.pages.html :refer [html]]
            [com.knocean.knode.pages.ontology.base :refer [ontology-result] :as base]
            [com.knocean.knode.pages.ontology.template :as tmp]))

(defn render-link
  [env iri]
  [:a {:href iri} (en/iri->name env iri)])

(defn local-link
  [env iri]
  (let [curie (en/iri->curie env iri)]
    (str "/ontology/" (string/replace curie ":" "_"))))

(defn render-subject
  [env iri]
  (let [curie (en/iri->curie env iri)]
    [:a
     {:href (if curie (str "/ontology/" (string/replace curie ":" "_")) iri)}
     curie
     " "
     (en/iri->name env iri)]))

(declare sort-statements)

(defn render-statement
  "Adapted from omn/render-statement."
  [{:keys [::en/env ::st/exact ::rdf/quad] :as state}]
  (cond
    exact (->> exact flatten (filter string?))
    :else
    (when (and (::rdf/oi quad) (not= (rdf/rdf "nil") (::rdf/oi quad)))
     (let [name (en/iri->name env (::rdf/oi quad))]
       (if (re-find #"\s" name)
         [:a {:href (::rdf/oi quad)} (str "'" name "'")]
         [:a {:href (::rdf/oi quad)} name])))))

(defn get-blank-states
  [env bnode]
  (let [trps (st/query {:select [:pi :oi :ob] :from [:states] :where [:= :sb bnode]})]
    (reduce 
      (fn [states trp]
        (conj 
          states 
          {::en/env env
           ::st/event ::st/statement
           ::rdf/quad {::rdf/pi (:pi trp) 
                       ::rdf/oi (:oi trp) 
                       ::rdf/ob (:ob trp)} 
           ::rdf/subject bnode}))
      [] trps)))

(defn render-omn
  [env bnode]
  (let [states (get-blank-states env bnode)]
    (->> states
         (group-by ::rdf/subject)
         (#(assoc %
                 ::rdf/subjects (->> states (map ::rdf/subject) distinct)
                 ::st/states []))
         sort-statements
         ::st/states
         (map render-statement)
         (concat [:span])
         (into []))))

(defn render-pair
  [env {:keys [pi oi ob ol] :as state}]
  [:li
   (render-link env pi)
   ": "
   (cond
     oi [:a {:href oi :property (en/iri->curie env pi)} (en/iri->name env oi)]
     ob (render-omn env ob)
     ol [:span {:property (en/iri->curie env pi)} ol])])

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
        states
        (->> [:= :si iri]
             st/select
             (map #(select-keys % [:si :sb :pi :oi :ob :ol :di :lt]))
             distinct)]
    [:div
     [:h2 (en/iri->curie env iri) " " (en/iri->name env iri)]
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
        :title (:project-name @state)
        :content
        [:div
         (when (auth/logged-in? req)
           (list [:div {:class "col-md-6"}
                  [:h3 "Add Term"]
                  [:form {:method "POST" :action "/ontology/validate-term"}
                   [:input {:type "hidden" :name "api-key" :value (str (get params "api-key"))}]
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
         [:h3 "Term List"]
         [:p
          "Other formats: "
          [:a {:href (str "/ontology/" (:project-name @state) ".ttl")} "Turtle (ttl)"]
          ", "
          [:a {:href (str "/ontology/" (:project-name @state) ".tsv")} "TSV (tsv)"]
          "."]
         [:ul (map (fn [s] [:li (render-subject env s)])
                   (base/all-subjects))]]}
     1 {:session session
        :title (en/iri->name env (first requested-iris))
        :content (render-subject-html (first requested-iris))}
     {:session session
      :title (:project-name @state)
      :content (map render-subject-html requested-iris)})))

;; -----------------------------------------
;; OMN rendering helper function
;; -----------------------------------------

(defn sort-statements
  "Adapted from omn/sort-statements."
  [coll]
  (loop [{:keys [::rdf/subjects ::depth] :as coll} coll]
    (if-let [subject (first subjects)]
      (recur
       (if (first (get coll subject))
         (let [coll (->> (get coll subject)
                         (map #(assoc % ::omn/omn true))
                         (assoc coll subject))
               states (get coll subject)
               state (first states)
               rdf-type        (omn/find-state states (rdf/rdf "type"))
               complement-of   (omn/find-state states (rdf/owl "complementOf"))
               intersection-of (omn/find-state states (rdf/owl "intersectionOf"))
               union-of        (omn/find-state states (rdf/owl "unionOf"))
               on-property     (omn/find-state states (rdf/owl "onProperty"))
               some-values     (omn/find-state states (rdf/owl "someValuesFrom"))
               all-values      (omn/find-state states (rdf/owl "allValuesFrom"))
               one-of          (omn/find-state states (rdf/owl "oneOf"))
               first-item      (omn/find-state states (rdf/rdf "first"))
               rest-item       (omn/find-state states (rdf/rdf "rest"))]
           (cond
             complement-of
             (let [ob (-> complement-of ::rdf/quad ::rdf/ob)]
               (-> coll
                   (update ::st/states conj (assoc rdf-type ::st/silent true))
                   (update ::st/states conj (assoc complement-of
                                                   ::st/before
                                                   (concat
                                                    [[:keyword "not"]
                                                     [:space " "]]
                                                    (when ob [[:symbol "("]]))))
                   (update subject (partial remove #{rdf-type complement-of}))
                   (update ::depth (fnil inc 0))
                   (assoc ::rdf/subjects (concat
                                          (when ob [ob])
                                          subjects))))

             one-of
             (let [ob (-> one-of ::rdf/quad ::rdf/ob)]
               (-> coll
                   (update ::st/states conj (assoc rdf-type ::st/silent true))
                   (update ::st/states conj (assoc one-of ::st/before [[:symbol "{"]]))
                   (update subject (partial remove #{rdf-type one-of}))
                   (update ::depth (fnil inc 0))
                   (assoc ::rdf/subjects (concat
                                          (when ob [ob])
                                          subjects))))

             (or union-of intersection-of)
             (-> coll
                 (update ::st/states conj (assoc rdf-type ::st/silent true))
                 (update ::st/states conj (assoc (or union-of intersection-of) ::st/silent true))
                 (update subject (partial remove #{rdf-type union-of intersection-of}))
                 (assoc ::rdf/subjects (concat
                                        (when-let [ob (-> union-of ::rdf/quad ::rdf/ob)] [ob])
                                        (when-let [ob (-> intersection-of ::rdf/quad ::rdf/ob)] [ob])
                                        subjects)))

             on-property
             (let [obp (-> on-property ::rdf/quad ::rdf/ob)
                   obv (-> (or some-values all-values) ::rdf/quad ::rdf/ob)
                   keyword (concat
                            (when obp [[:symbol ")"]])
                            [[:space " "]
                             [:keyword (if some-values "some" "only")]
                             [:space " "]]
                            (when obv [[:symbol "("]]))]
               (-> coll
                   (update ::st/states conj (assoc on-property ::st/before (when obp [[:symbol "("]])))
                   (update ::st/states conj (assoc rdf-type ::st/exact keyword))
                   (update subject (partial remove #{rdf-type on-property}))
                   (assoc ::rdf/subjects (concat
                                          (when obp [obp])
                                          (when obv [obv])
                                          subjects))))

             (or some-values all-values)
             (let [obv (-> (or some-values all-values) ::rdf/quad ::rdf/ob)]
               (-> coll
                   (update ::st/states conj (assoc (or some-values all-values) ::st/after (when obv [[:symbol ")"]])))
                   (update subject (partial remove #{some-values all-values}))))

             first-item
             (if-let [ob (-> first-item ::rdf/quad ::rdf/ob)]
               (-> coll
                   (update ::st/states conj (assoc first-item ::st/silent true))
                   (update subject (partial remove #{first-item}))
                   (assoc ::rdf/subjects (concat [ob] subjects)))
               (-> coll
                   (update ::st/states conj first-item)
                   (update subject (partial remove #{first-item}))))

             rest-item
             (let [collection
                   (->> coll
                        ::st/states
                        reverse
                        (map ::rdf/quad)
                        (map ::rdf/pi)
                        (filter #{(rdf/owl "unionOf") (rdf/owl "intersectionOf") (rdf/owl "oneOf")})
                        first)]
               (if-let [ob (-> rest-item ::rdf/quad ::rdf/ob)]
                 (-> coll
                     (update ::st/states conj (assoc rest-item
                                                     ::st/exact
                                                     (condp = collection
                                                       (rdf/owl "unionOf") [[:space " "] [:keyword "or"] [:space " "]]
                                                       (rdf/owl "intersectionOf") [[:space " "] [:keyword "and"] [:space " "]]
                                                       (rdf/owl "oneOf") [[:symbol ","] [:space " "]])))
                     (update subject (partial remove #{rest-item}))
                     (assoc ::rdf/subjects (concat [ob] subjects)))
                 (if (and depth (> depth 0))
                   (-> coll
                       (update ::st/states conj (assoc rest-item ::st/exact [(if (= collection (rdf/owl "oneOf")) [:symbol "}"] [:symbol ")"])]))
                       (update subject (partial remove #{rest-item}))
                       (update ::depth dec))
                   (-> coll
                       (update ::st/states conj (assoc rest-item ::st/silent true))
                       (update subject (partial remove #{rest-item}))))))

             ; this should never be reached?
             :else
             (-> coll
                 (update ::st/states conj state)
                 (update subject rest))))
         ; no more states for this subject
         (-> coll
             (dissoc subject)
             (update ::rdf/subjects rest))))
      ; no more subjects
      (let [states (::st/states coll)
            last-state (last states)]
        (assoc
         coll
         ::st/states
         (conj (vec (butlast states))
               (-> last-state
                   (dissoc ::st/silent)
                   (assoc ::last true))))))))

