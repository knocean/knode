(ns knode.cli
  (:require
   [clojure.java.io :as io]
   [clojure.string :as string]

   [org.httpkit.server :as server]
   [compojure.route :as route]
   [hiccup.core :as html]

   [knode.core :as in]
   [knode.emit :as out])
  (:use [compojure.core :only [defroutes GET POST DELETE ANY context]])
  (:gen-class))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; Filesystem manipulation
(defn tsv-rows [filename]
  (with-open [reader (io/reader filename)]
    (doall
     (let [lns (map #(string/split % #"\t") (line-seq reader))
           headers (map #(keyword (string/lower-case %)) (first lns))]
       (map #(into {} (map vector headers %)) (rest lns))))))

(defn add-tsv-to-env [env filename]
  (in/add-labels-to-environment env (tsv-rows filename)))

(defn parse-kn-file [filename]
  (with-open [reader (io/reader filename)]
    (doall (in/parse-lines (line-seq reader)))))

(defn parse-files [filenames]
  (in/expand-env+forms
   (reduce
    (fn [state fname]
      (cond (re-find #"\.tsv" fname)
            (assoc state :env (add-tsv-to-env (:env state) fname))
            (re-find #"\.kn" fname)
            (assoc state :forms (concat (:forms state) (parse-kn-file fname)))))
    {:env {} :forms ()} filenames)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; Server infrastructure
(def state (atom {}))

(defn render-html [req]
  {:status 200
   :headers {"Content-Type" "text/html"}
   :body (html/html (out/emit-html @state))})

(defn render-ttl [req]
  {:status 200
   :headers {"Content-Type" "text/plain"}
   :body (out/emit-ttl @state)})

(defn render-json [req]
  {:status 200
   :headers {"Content-Type" "application/json"}
   :body "{\"status\": \"still TODO\"}"})

(defroutes knode-routes
  (GET "/" [] render-html)
  (GET "/ttl" [] render-ttl)
  (GET "/json" [] render-json))

(defn serve [port directory]
  (let [dir (str directory "ontology/")]
    (println "Parsing from " dir "...")
    (let [fnames (vec (reverse (map #(.getPath %) (rest (file-seq (io/file dir))))))]
      (swap! state (fn [_] (parse-files fnames)))))

  (println "Listening on" port "...")
  (server/run-server knode-routes {:port (read-string port)}))

(defn test [source-dir expected-dir]
  (println "TODO - slurp files from source dir, check that the outputs match expected-dir"))

(defn -main [task & args]
  (println "TODO - " task args))
