(ns knode.server
  (:require
   [clojure.java.io :as io]
   [clojure.string :as string]

   [org.httpkit.server :as server]
   [compojure.route :as route]
   [markdown.core :as md]
   [hiccup.page :as html]

   [knode.core :as core]
   [knode.state :as state]
   [knode.emit :as emit]
   [knode.util :as util])
  (:use [compojure.core :only [defroutes GET POST DELETE ANY context]]))

(defn render-index
  [req]
  {:status 200
   :headers {"Content-Type" "text/html"}
   :body (html/html5
          (md/md-to-html-string
           (slurp (io/resource "README.md"))))})

(defn render-html
  [req]
  {:status 200
   :headers {"Content-Type" "text/html"}
   :body "BROKEN"}) ; (html/html (out/emit-html @state))}))

(defn render-ttl
  [req]
  (let [iri (str @state/root-iri (get-in req [:route-params :id]))
        term (get-in @state/state [:terms iri])]
    {:status 200
     :headers {"Content-Type" "text/plain"}
     :body
     (emit/emit-ttl2
      (:context @state/state)
      (:subject term)
      (:blocks term))}))

(defroutes knode-routes
  (GET "/" [] render-index)
  (GET "/index.html" [] render-index)
  (GET "/:id.html" [id] render-html)
  (GET "/:id.ttl" [id] render-ttl))

(defn serve
  "Load data and serve pages on a given port."
  [dir port]
  (println "Listening on" port "...")
  (server/run-server knode-routes {:port (read-string port)}))
