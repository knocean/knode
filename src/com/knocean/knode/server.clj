(ns com.knocean.knode.server
  (:require
   [clojure.java.io :as io]
   [clojure.string :as string]
   [org.httpkit.server :as httpkit]
   [bidi.bidi :as bidi]
   [bidi.ring :as bring]
   [ring.middleware.session :refer [wrap-session]]
   [ring.middleware.params :refer [wrap-params]]

   [markdown.core :as md]
   [yaml.core :as yaml]

   [com.knocean.knode.state :refer [state]]
   [com.knocean.knode.pages.html :refer [html]]
   [com.knocean.knode.pages.authentication :as auth]
   [com.knocean.knode.pages.ontology.core :as ontology]
   [com.knocean.knode.pages.term-status :as stat]
   [com.knocean.knode.pages.linked-data-fragments.core :as ldf]
   [com.knocean.knode.pages.subject :as subject]))

(defn not-found
  [request]
  (html
   {:status 404
    :title "404 Page not found"
    :content "404 Page not found"}))

(defn render-doc
  [req path]
  (let [file (io/file path)]
    (when (.exists file)
      (let [[_ _ header content] (re-find #"(?s)(---(.*?)---)(.*)" (slurp file))
            metadata (yaml/parse-string header)]
        (html
         {:title (:title metadata)
          :content (md/md-to-html-string content)})))))

(defn index
  [request]
  (render-doc request (:readme @state)))

(def routes
  [""
   (concat
    ;auth/routes
    ;subject/routes
    ontology/routes
    ;stat/routes
    ;ldf/routes
    [["" (bring/redirect index)]
     ["/" index]
     ["/index.html" (bring/redirect index)]
     ["/doc/" (bring/redirect index)]
     ["/doc/index.html" index]
     ["/doc/api.html" #(render-doc % (str (:project-dir @state) "/doc/api.md"))]
     ["/" (bring/->ResourcesMaybe {:prefix "public/"})]
     [true not-found]])])

(defn start
  "Run an HTTP server."
  []
  (swap!
   state
   assoc
   :server
   (httpkit/run-server
    (->> routes
         bring/make-handler
         wrap-session
         wrap-params)
    {:port (Integer. (:port @state))}))
  (println "Listening on" (:port @state) "..."))

(defn stop
  []
  (when (:server @state)
    ((:server @state) :timeout 100)
    (swap! state dissoc :server)
    (println "Server stopped...")))

(defn restart
  []
  (stop)
  (start))
