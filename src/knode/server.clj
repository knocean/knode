(ns knode.server
  (:require
   [clojure.java.io :as io]
   [clojure.string :as string]

   [org.httpkit.server :as server]
   [compojure.route :as route]
   [hiccup.core :as hiccup]
   [hiccup.page :as page]

   [knode.state :refer [state]]
   [knode.emit :as emit])
  (:use [compojure.core :only [defroutes GET]]))

(defn render-html
  [req]
  (let [iri (str (:root-iri @state) (get-in req [:route-params :id]))
        term (get-in @state [:terms iri])]
    {:status 200
     :headers {"Content-Type" "text/html"}
     :body
     (page/html5
      (hiccup/html
       (emit/emit-rdfa
        (:context @state)
        (:subject term)
        (:blocks term))))}))

(defn render-ttl
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
  (GET "/:id" [id] render-html))

(defn serve
  "Load data and serve pages on a given port."
  []
  (println "Listening on" (:port @state) "...")
  (server/run-server knode-routes {:port (:port @state)}))
