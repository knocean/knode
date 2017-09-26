(ns knode.server.handlers
  (:require [clojure.string :as string]

            [compojure.route :as route]
            [bidi.bidi :as bidi]
            [bidi.ring :as ring]
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
   {:internal-error (http-error
                     500
                     "Internal Error"
                     "The server errored in some way. This isn't your fault, but we still can't process your request.")
    :not-found (http-error
                404 "Not Found"
                "Sorry, the page you requested was not found.")}))

(def routes-data
  (atom ["/" {"" (ring/->ResourcesMaybe {:prefix "public/"})}]))

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

(defn bidi-method [method-name path]
  (if (= :any method-name)
    path
    (vec (cons method-name path))))

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

(defmulti intern-handler-fn! (fn ([p _ _] (class p)) ([_ p _ _] (class p))))

(defmethod intern-handler-fn! java.lang.String
  ([path name f] (intern-handler-fn! [path] name f))
  ([method path name f] (intern-handler-fn! method [path] name f)))

(defmethod intern-handler-fn! clojure.lang.PersistentVector
  ([paths name f] (intern-handler-fn! :any paths name f))
  ([method paths name f]
   (when (contains? @handler-table name) (warn (str "Overriding handler name " name)))
   (doseq [p paths]
     (swap!
      routes-data
      (fn [dat]
        [(first dat)
         (insert-new-handler
          (second dat)
          (bidi-method method (string->bidi-path p))
          name)])))
   (swap! handler-table assoc name f)
   nil))

(def link-to (partial bidi/path-for @routes-data))

(defn route-request
  [req]
  (if-let [res (bidi/match-route @routes-data (:uri req) :request-method (:request-method req))]
    res
    {:handler :not-found}))

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
      (if (keyword? (:handler routed))
        ((get @handler-table (:handler routed)) routed-req)
        ((:handler routed) routed-req))
      (catch Exception e
        (try
          ((get @handler-table :internal-error) routed-req)
          (catch Exception e
            {:status 500
             :headers {"Content-Type" "text/plain"}
             :body "Something went really, horrifically wrong with that request."}))))))

(defonce ts (atom nil))
