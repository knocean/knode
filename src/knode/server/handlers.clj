(ns knode.server.handlers
  (:require [clojure.string :as string]

            [compojure.route :as route]
            [bidi.bidi :as bidi]
            [trivial-warning.core :refer [warn]]

            [knode.server.template :refer [base-template]]))

(defn http-error [error-code name blurb]
  (fn [req]
    {:status error-code
     :headers {"Content-Type" "text/html; charset=utf-8"}
     :body (base-template
            (:session req)
            {:title name
             :content [:div [:h1 name]
                       [:p blurb]
                       [:p [:a {:href "/"} "Return to home page."]]]})}))

(def handler-table
  (atom
   {:static-resource (route/resources "")
    :internal-error (http-error
                     500
                     "Internal Error"
                     "The server errored in some way. This isn't your fault, but we still can't process your request.")
    :not-found (http-error
                404 "Not Found"
                "Sorry, the page you requested was not found.")}))

(def routes-data
  (atom ["/" {}]))

(defn string->bidi-path
  [s]
  (let [raw (->> (string/split s #"/")
                 (filter #(not (empty? %)))
                 (map #(if (string/starts-with? % ":") (keyword (subs % 1)) %)))]
    (loop [[a b & rem] raw
           memo []]
      (cond (nil? a) (conj memo "")
            (and (nil? b) (empty? memo)) (conj memo a)
            (and (nil? b) (= a "*")) (conj memo [[#"(.*)" :*] ""])
            (nil? b) (conj memo (str "/" a))
            (keyword? b) (recur rem (conj memo [(str a "/") b]))
            (empty? memo) (recur (cons b rem) (conj memo a))
            :else (recur (cons b rem) (conj memo (str "/" a)))))))

(defn insert-new-handler
  [path-map new-path handler-tag]
  ((fn rec [map [a & path]]
     (cond (and (contains? map a) (nil? path))
           (let [binding (get map a)]
             (when (or (keyword? binding)
                       (and (map? binding) (contains? binding "")))
               (warn (str "Overriding handler " (get map a))))
             (if (keyword? binding)
               (assoc map a handler-tag)
               (assoc map a (assoc binding "" handler-tag))))
           (contains? map a) (let [res (get map a)
                                   next (if (keyword? res) {"" res} res)]
                               (assoc map a (rec next path)))
           (nil? path) (assoc map a handler-tag)
           :else (assoc map a (rec {} path))))
   path-map new-path))

(defmulti intern-handler-fn! (fn [p & _] (class p)))

(defmethod intern-handler-fn! java.lang.String
  ([path name f] (intern-handler-fn! [path] name f))
  ([path name method f] (intern-handler-fn! [path] name method f)))

(defmethod intern-handler-fn! clojure.lang.PersistentVector
  ([paths name f] (intern-handler-fn! paths name :get f))
  ([paths name method f]
   (when (contains? @handler-table name) (warn (str "Overriding handler name " name)))
   (map
    #(swap!
      routes-data
      (fn [dat]
        [(first dat)
         (insert-new-handler
          (second dat)
          (string->bidi-path %)
          name)]))
    paths)
   (swap! handler-table assoc name f)
   nil))

;; (intern-handler-fn!
;;  "/test" :test-page
;;  (fn [req]
;;    (println "REQUEST FROM BIDI:" (str req))))

;; (intern-handler-fn!
;;  "/foo/:bar/baz" :path-params-test-page
;;  (fn [req]
;;    (println "REQUEST FROM BIDI ... WITH PATH PARAMS:" (str req))))

;; (defn tapper [req] (println "A REQUEST TO '" (get req :uri) "': " req))
;; (intern-handler-fn! "/foo" :tapper tapper)
;; (intern-handler-fn! "/foo" :tapper {:method :get})

;; Static assets map to resources/public/*
;; You probably shouldn't work on caching them, in all honesty. Proxies do that better than the JVM.
;; Also, bidi seems to provide some static resource handling stuff at https://github.com/juxt/bidi#files
(defn static-resource-exists?
  [uri]
  "TODO"
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
