(ns knode.server.handlers
  (:require [clojure.string :as string]

            [compojure.route :as route]
            [bidi.bidi :as bidi]

            [knode.server.template :refer [base-template]]))

(defn error-template [name blurb]
  #(base-template
    (:session %)
    {:title name
     :content [:div [:h1 name]
               [:p blurb]
               [:p [:a {:href "/"} "Return to home page."]]]}))

(def handler-table
  (atom
   {:static-resource (route/resources "")
    :internal-error (fn [req]
                      {:status 500
                       :headers {"Content-Type" "text/html; charset=utf-8"}
                       :body (error-template
                              "Internal Error"
                              "The server errored in some way. This isn't your fault, but we still can't process your request.")})
    :not-found (fn [req]
                 {:status 404
                  :heades {"Content-Type" "text/html; charset=utf-8"}
                  :body (error-template
                         "Not Found"
                         "Sorry, the page you requested was not found.")})}))

(def routes-data
  (atom ["/" {}]))

(defn string->bidi-path
  [s]
  (let [raw (->> (string/split s #"/")
                 (filter #(not (empty? %)))
                 (map #(if (string/starts-with? % ":") (keyword (subs % 1)) %)))]
    (loop [[a b & rem] raw
           memo []]
      (cond (and (nil? b) (empty? memo)) (conj memo a)
            (nil? b) (conj memo (str "/" a))
            (keyword? b) (recur rem (conj memo [(str a "/") b]))
            (empty? memo) (recur (cons b rem) (conj memo a))
            :else (recur (cons b rem) (conj memo (str "/" a)))))))

(defn insert-new-handler
  [path-map new-path handler-tag]
  [(first path-map)
   ((fn rec [map [a & path]]
      (cond (nil? path) (do (when (contains? map a) (println "WARNING: Overriding handler" (str (get map a))))
                            (assoc map a handler-tag))
            (contains? map a) (let [res (get map a)
                                    next (if (keyword? res) {"" res} res)]
                                (assoc map a (rec next path)))
            :else (assoc map a (rec {} path))))
    (second path-map) new-path)])

(defn intern-handler-fn!
  ([path fn] (intern-handler-fn! path (keyword (gensym "HANDLER_")) fn))
  ([path name fn]
   (swap! routes-data #(insert-new-handler % (string->bidi-path path) name))
   (swap! handler-table assoc name fn)
   nil))

(intern-handler-fn!
 "/test" :test-page
 (fn [req]
   (println "REQUEST FROM BIDI:" (str req))))

(intern-handler-fn!
 "/foo/:bar/baz" :path-params-test-page
 (fn [req]
   (println "REQUEST FROM BIDI ... WITH PATH PARAMS:" (str req))))

;; Static assets map to resources/public/*
;; You probably shouldn't work on caching them, in all honesty. Proxies do that better than the JVM.
;; Also, bidi seems to provide some static resource handling stuff at https://github.com/juxt/bidi#files
(defn static-resource-exists?
  [uri]
  nil)

(def link-to (partial bidi/path-for @routes-data))

(defn route-request
  [req]
  (if-let [res (bidi/match-route @routes-data (:uri req))]
    res
    (if (static-resource-exists? (:uri req))
      {:handler :static-resource}
      {:handler :not-found})))

(defn routes-handler
  [req]
  (let [routed (route-request req)
        route-params (->> routed :route-params
                          ; HTTP-kit parses parameters out to string maps, not keyword maps. Lets be consistent.
                          (map (fn [[k v]] [(name k) v]))
                          (into {}))
        routed-req (assoc
                    req
                    :route-params (or route-params {})
                    :params (merge (:params req) route-params))]
    (try
      ((get @handler-table (:handler routed)) routed-req)
      (catch Exception e
        (try
          ((get @handler-table :internal-error) routed-req)
          (catch Exception e
            {:status 500
             :headers {"Content-Type" "text/plain"}
             :body "Something went really, horrifically wrong with that request."}))))))

(defonce ts (atom nil))

;; (do
;;   (@ts :timeout 10)
;;   (reset!
;;    ts (httpkit/run-server
;;        (->> routes-handler
;;             wrap-session
;;             wrap-params
;;             wrap-head)
;;        {:port 4444})))
