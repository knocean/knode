(ns knode.server
  (:require
   [clojure.java.io :as io]
   [clojure.string :as string]

   [org.httpkit.server :as server]
   [compojure.route :as route]
   [hiccup.core :as hiccup]
   [markdown.core :as md]
   [yaml.core :as yaml]

   [knode.state :refer [state]]
   [knode.emit :as emit])
  (:use [compojure.core :only [defroutes GET]]))

(defn base-template
  "Given a title and a Hiccup vector,
   load an HTML template, insert the values, and return the HTML string."
  [title content]
  (-> "base.html"
      io/resource
      slurp
      (string/replace "{{TITLE}}" title)
      (string/replace-first "{{CONTENT}}" (hiccup/html content))))

(defn render-html
  "Given a request, try to find a matching term,
   and return a response map for HTML."
  [req]
  (let [id (get-in req [:route-params :id])
        iri (str (:root-iri @state) "ontology/" id)
        term (get-in @state [:terms iri])
        subject (:subject term)
        label (get-in @state [:env :iri-labels (:iri subject)])]
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
  (let [iri (str (:root-iri @state) "ontology/" (get-in req [:route-params :id]))
        term (get-in @state [:terms iri])]
    (when term
      {:status 200
       :headers {"Content-Type" "text/turtle"}
       :body
       (emit/emit-ttl
        (:context @state)
        (:subject term)
        (:blocks term))})))

(defn render-doc
  [doc]
  {:status 200
   :headers {"Content-Type" "text/html"}
   :body
   (let [path (str (:root-dir @state) "doc/" doc ".md")
         file (io/file path)]
     (when (.exists file)
       (let [[_ _ header content] (re-find #"(?s)(---(.*?)---)(.*)" (slurp file))
             metadata (yaml/parse-string header)]
         (base-template
          (:title metadata)
          (md/md-to-html-string content)))))})

(defroutes knode-routes
  ; ontology terms
  (GET "/ontology/:id.html" [id] render-html)
  (GET "/ontology/:id.ttl" [id] render-ttl)
  (GET "/ontology/:id" [id] render-html)

  ; doc directory
  (GET "/doc/:doc.html" [doc] (render-doc doc))
  (GET "/index.html" [] (render-doc "index"))
  (GET "/" [] (render-doc "index"))

  ; static resources
  (route/resources "")

  ; not found
  (route/not-found
   (base-template
    "Not Found"
    [:div
     [:h1 "Not Found"]
     [:p "Sorry, the page you requested was not found."]
     [:p [:a {:href "/"} "Return to home page."]]])))

; Start/Restart http://www.http-kit.org/server.html#stop-server
(defonce server (atom nil))

(defn serve
  "Load data and serve pages on a given port."
  []
  (println "Listening on" (:port @state) "...")
  (reset! server (server/run-server knode-routes {:port (:port @state)})))

(defn stop
  []
  (when-not (nil? @server)
    (@server :timeout 100)
    (reset! server nil)))

(defn restart
  []
  (stop)
  (serve))
