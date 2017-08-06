(ns knode.server.util
  (:require
   [clojure.string :as string]
   [clojure.data.json :as json]
   [clojure.data.csv :as csv]

   [knode.state :refer [state]]))

(defn seq->tsv-string
  [rows]
  (with-out-str (csv/write-csv *out* rows :separator \tab)))

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

(def mimetype-table
  {"text/html" "html"
   "text/turtle" "ttl"
   "text/tab-separated-values" "tsv"
   "application/json" "json"
   "application/edn" "edn"})

(defn format->content-type
  ([format] (format->content-type format "json"))
  ([format default] {"Content-Type" (get (clojure.set/map-invert mimetype-table) format default)}))

(defn parse-request-output-format
  [{:keys [params uri] :as req}]
  (let [accept (string/split (get-in req [:headers "accept"] "") #", ?")]
    (or (when (find params "output-format")
          (string/lower-case (get params "output-format")))
        (when (find params "format")
          (string/lower-case (get params "format")))
        (get mimetype-table (first accept))
        (get mimetype-table (second accept))
        (when-let [[_ extension]
                   (re-matches
                    #"^.*\.(html|ttl|json|tsv)$"
                    (string/lower-case (or uri "")))]
          extension))))

(defn parse-request-method
  [{:keys [params request-method] :as req}]
  (if (find params "method")
    (-> (get params "method" "") string/lower-case keyword)
    request-method))
