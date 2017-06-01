(ns knode.server.util
  (:require
   [clojure.string :as string]
   [clojure.data.json :as json]

   [knode.state :refer [state]]))

(defn re-root
  [state req uri]
  (string/replace
   uri
   (:root-iri state)
   (str "https://" (get-in req [:headers "host"] "localhost") "/")))

(defn get-terms
  [state]
  (->> state
       :terms
       vals
       (sort-by #(->> % :subject :iri))))

(defn json-error
  [status & messages]
  {:status status
   :headers {"Content-Type" "application/json"}
   :body
   (json/write-str
    {:error (string/join " " messages)}
    :escape-slash false)})

(defn login?
  [req]
  (let [host (get-in req [:headers "host"])
        google-id (:google-client-id @state)
        google-secret (:google-client-secret @state)]
    (and (string? host)
         (string? google-id)
         (not (string/blank? google-id))
         (string? google-secret)
         (not (string/blank? google-secret)))))

(defn developer?
  [req]
  (or
   ; Google login
   (and (get-in req [:session :email])
        (not (string/blank? (:developers @state)))
        (contains?
         (set (string/split (:developers @state) #"\s+"))
         (get-in req [:session :email])))

   ; API Key header
   (let [dev-key (:dev-key @state)
         api-key (get-in req [:headers "x-api-key"])]
     (and (string? dev-key)
          (string? api-key)
          (not (string/blank? dev-key))
          (= dev-key api-key)))))
