(ns knode.server
  (:require
   [clojure.java.io :as io]
   [clojure.string :as string]

   [org.httpkit.server :as server]
   [compojure.route :as route]
   [hiccup.core :as hiccup]

   [knode.state :refer [state]]
   [knode.emit :as emit])
  (:use [compojure.core :only [defroutes GET]]))

(defn base-template
  [title content]
  (-> (str (:root-dir @state) "www/index.html")
      slurp
      (string/replace-first
       #"<title>.*</title>"
       (format
        "<title>%s</title>"
        title))
      (string/replace-first
       #"(?s)<div id=\"content\".*\/content -->"
       (format
        "<div id=\"content\">%s</div"
        (hiccup/html content)))))

(defn render-html
  "Given a request, try to find a matching term,
   and return a response map for HTML."
  [req]
  (let [id (get-in req [:route-params :id])
        iri (str (:root-iri @state) id)
        term (get-in @state [:terms iri])
        subject (:subject term)
        label (get-in @state [:iri-labels (:iri subject)])]
    (when term
      {:status 200
       :headers {"Content-Type" "text/html"}
       :body
       (base-template
        label
        [:div
         [:h2 (:curie subject) " " label]
         [:p [:a {:href iri} iri]]
         (doall
          (emit/emit-rdfa
           (:context @state)
           (:subject term)
           (:blocks term)))
         [:p
          "Other formats: "
          [:a
           {:href (str (string/replace id #".html^" "") ".ttl")}
           "Turtle (ttl)"]
          "."]])})))

(defn render-ttl
  "Given a request, try to find a matching term,
   and return a response map for Turtle."
  [req]
  (let [iri (str (:root-iri @state) (get-in req [:route-params :id]))
        term (get-in @state [:terms iri])]
    {:status 200
     :headers {"Content-Type" "text/turtle"}
     :body
     (emit/emit-ttl
      (:context @state)
      (:subject term)
      (:blocks term))}))

(defroutes knode-routes
  (route/files "" {:root (str (:root-dir @state) "www/")})
  (GET "/:id.html" [id] render-html)
  (GET "/:id.ttl" [id] render-ttl)
  (GET "/:id" [id] render-html)
  (route/not-found
   (base-template
    "Not Found"
    [:div
     [:h1 "Not Found"]
     [:p "Sorry, the page you requested was not found."]
     [:p [:a {:href "/"} "Return to home page."]]])))

(defn serve
  "Load data and serve pages on a given port."
  []
  (println "Listening on" (:port @state) "...")
  (server/run-server knode-routes {:port (:port @state)}))
