(ns com.knocean.knode.pages.tree-view
  (:require [clojure.data.json :as json]
            [clojure.data :refer [diff]]
            [clojure.string :as s]
            [hiccup.page :refer [html5]]

            [org.knotation.environment :as en]
            [com.knocean.knode.state :as st]
            [com.knocean.knode.pages.html :refer [html]]
            [com.knocean.knode.pages.resources :as r]
            [com.knocean.knode.util :as util]))

;; TODO: handle deprecated entities
(def owl-deprecated "http://www.w3.org/2002/07/owl#deprecated")
(def rdfs-label "http://www.w3.org/2000/01/rdf-schema#label")
(def rdfs-subclass "http://www.w3.org/2000/01/rdf-schema#subClassOf")

(defn get-children
  "Given an IRI to get children of (oi), 
   the resource name to look in (rt), 
   and at least one selector, 
   return the query results for all children of oi."
  [oi rt & selectors]
  (->> {:select selectors
        :from [:states] 
        :where [:and 
                 [:= :rt rt] 
                 [:= :pi rdfs-subclass] 
                 [:= :oi oi]]}
       st/query
       distinct))

(defn get-label
  "Given a subject IRI (si) and a resource name (rt),
   return the label for the si."
  [si rt]
  (->> {:select [:ol]
        :from [:states]
        :where [:and
                 [:= :rt rt]
                 [:= :pi rdfs-label]
                 [:= :si si]]}
       st/query
       distinct
       first
       :ol))

(defn get-inspire-node
  "Given an IRI and a resource name,
   return the inspire node for that entity."
  [iri resource]
  [{:text (get-label iri resource)
    :id (s/replace iri #"#" "%23") 
    :children
    (->> (get-children iri resource :si)
         empty?
         not)}])

(defn place-other-last
  "Given a vector of nodes,
   place any 'other' nodes at the end of the list."
  [nodes]
  (let [other-node (filter #(s/includes? (:text %) "other") nodes)
        rest-nodes (remove #(s/includes? (:text %) "other") nodes)]
    (concat rest-nodes other-node)))

(defn get-sorted-nodes
  "Given a vector of IRIs and a resource name,
   return a vector of sorted inspire nodes."
  [iris resource]
  (->> iris
       (reduce 
        (fn [v i] 
          (concat v (get-inspire-node i resource)))
        [])
       (sort-by #(s/lower-case (:text %)))
       place-other-last))

;; sort alphabetically
;; put 'other' node at bottom of list
(defn get-child-inspire-nodes
  "Given a request with an IRI or CURIE,
   return a vector of sorted inspire nodes of the IRI in the request."
  [{:keys [session params] :as req}]
  (if-let [resource (:resource params)]
    (if-let [iri (or (:iri req)
                     (get-in req [:params "iri"])
                     (when-let [curie (get-in req [:params "curie"])]
                       (en/curie->iri (st/latest-env) curie)))]
      (if-let [children (->> (get-children iri resource :si)
                          (map #(:si %)))]
        {:status 200
         :headers {"Content-Type" "application/json"}
         :body 
         (json/write-str (get-sorted-nodes children resource))}
         ;; How to handle errors?
         {:status 400
          :headers {"Content-Type" "application/json"}
          :body "No children to show"})
      {:status 400
       :headers {"Content-Type" "application/json"}
       :body "Missing IRI"})
    {:status 400
     :headers {"Content-Type" "application/json"}
     :body "Missing resource name"}))

(defn tree-page
  [{:keys [session params] :as req}]
  (let [resource (:resource params)]
    (html
      {:session session
       :title (format "%s - Tree View" resource)
       :content
       [:div {:class "container-fluid"}
         [:div {:class "row"}
          [:div {:class "col-md-5"}
            [:div.subject
              [:h2 resource]]
            [:div {:class "tree"}]]
          [:div {:class "col-md-7"}
            [:div {:class "info"}]]]
          [:link 
            {:href "/assets/inspire-tree/inspire-tree-light.min.css" 
             :rel "stylesheet"}]
          [:script {:src "/assets/jquery.min.js"}]
          [:script 
            {:src "/assets/inspire-tree/lodash.min.js" 
             :type "text/javascript" 
             :charset "utf-8"}]
          [:script {:src "/assets/inspire-tree/inspire-tree.min.js"}]
          [:script {:src "/assets/inspire-tree/inspire-tree-dom.min.js"}]
          [:script {:type "text/javascript" :charset "utf-8"}
            (format "var rt = '%s';" resource)]
          [:script {:src "/assets/inspire-tree/inspire-tree.js"}]]})))

(defn resource-entry
  "Given a map of resource details, return the resource as an HTML element with 
   a link to its tree view."
  [{:keys [label title description homepage]}]
  [:div.resource
   [:h3 [:a {:href (str "/tree/" label)} title]]
   [:p description]
   (when homepage [:p [:a {:href homepage} homepage]])])

(defn trees-page
  "Given a map with a session key, 
   generate a list of the resource tree views."
  [{:keys [session] :as req}]
  (html
    {:session session
     :title "Tree Views"
     :content
     [:div
      [:h2 "Tree Views"]
      (->> (st/query {:select [:*] :from [:resources] :order-by [:id]})
           (map resource-entry)
           (into [:div]))]}))

;; put subject page on right-hand side of page when you click on thing

(defn get-info
  "Given a map with session and params keys.
   get the info HTML for the IRI specified by the request."
  [{:keys [session params] :as req}]
  (if-let [resource (:resource params)]
    (if-let [iri (or (:iri req)
                     (get-in req [:params "iri"])
                     (when-let [curie (get-in req [:params "curie"])]
                       (en/curie->iri (st/latest-env) curie)))]
      {:status 200
       :headers {"Content-Type" "text/html; charset=utf-8"}
       :body (html5 (:content (r/subject-html iri resource)))})))

(def routes 
  [["/trees" trees-page]
   ["/trees/" trees-page]
   [["/tree/" :resource] #(tree-page %)]
   [["/tree/" :resource "/children"] #(get-child-inspire-nodes %)]
   [["/tree/" :resource "/info"] #(get-info %)]])
