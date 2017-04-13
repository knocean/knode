(ns knode.cli
  (:require
   [clojure.java.io :as io]
   [clojure.string :as string]

   [org.httpkit.server :as server]
   [compojure.route :as route]
   [hiccup.core :as html]

   [knode.core :as core]
   [knode.emit :as emit]
   [knode.util :as util])
  (:use [compojure.core :only [defroutes GET POST DELETE ANY context]])
  (:gen-class))

(def state (atom {}))

(defn load-state
  "Given a directory, load data into the atoms."
  [dir]
  (with-open [reader (io/reader (str dir "context.kn"))]
    (let [[env blocks] (core/process-lines {} (line-seq reader))]
      (swap! state assoc :context blocks)
      (swap! state assoc :env env)))
  (with-open [reader (io/reader (str dir "external.tsv"))]
    (->> reader
         line-seq
         rest
         (map #(string/split % #"\t"))
         (map (fn [[label target _]] (str "@label " label ": " target)))
         (core/process-lines (:env @state))
         first
         (swap! state assoc :env)))
  (with-open [reader (io/reader (str dir "index.tsv"))]
    (->> reader
         line-seq
         rest
         (map #(string/split % #"\t" 3))
         (map (fn [[curie label _]] (str "@label " label ": " curie)))
         (core/process-lines (:env @state))
         first
         (swap! state assoc :env)))
  (with-open [reader (io/reader (str dir "ontie.kn"))]
    (->> (core/process-lines (:env @state) (line-seq reader))
         second
         (partition-by :subject)
         (partition 2)
         (map
          (fn [[[subject] blocks]]
            [(get-in subject [:subject :iri])
             {:subject subject
              :blocks blocks}]))
         (into {})
         (swap! state assoc :terms))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; Server infrastructure
(def root-iri (atom ""))

(defn render-index
  [req]
  {:status 200
   :headers {"Content-Type" "text/html"}
   :body "INDEX"})

(defn render-html
  [req]
  {:status 200
   :headers {"Content-Type" "text/html"}
   :body "BROKEN"}) ; (html/html (out/emit-html @state))}))

(defn render-ttl
  [req]
  (let [iri (str @root-iri (get-in req [:route-params :id]))
        term (get-in @state [:terms iri])]
    {:status 200
     :headers {"Content-Type" "text/plain"}
     :body
     (emit/emit-ttl2
      (:context @state)
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
  (println "Loading data from" dir "...")
  (load-state dir)
  (println "Listening on" port "...")
  (server/run-server knode-routes {:port (read-string port)}))

;; TODO: test command
(defn -main [task dir root port & args]
  (reset! root-iri root)
  (case task
    "serve" (serve dir port)
    "test" (println "TODO")))
