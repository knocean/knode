(ns com.knocean.knode.pages.mimetypes
  (:require
   [clojure.string :as string]
   [clojure.set :as set]))

(def mimetype-table
  {"text/html" "html"
   "text/turtle" "ttl"
   "text/tab-separated-values" "tsv"
   "application/json" "json"
   "application/edn" "edn"})

(defn content-type->format [content-type]
  (get mimetype-table content-type))

(defn req->format-link
  [req format]
  [:a
   {:href
    (str
     (:uri req) "?"
     (string/join
      "&" (map #(str (first %) "=" (second %))
               (assoc (:query-params req) "output-format" format))))}
   format])

(defn format->content-type
  ([format] (format->content-type format "text/html"))
  ([format default]
   (get (set/map-invert mimetype-table) format default)))

(defn req->output-format
  [{:keys [params uri headers] :as req}]
  (or (when (find params "output-format")
        (string/lower-case (get params "output-format")))
      (when (find params "format")
        (string/lower-case (get params "format")))
      (when-let [[_ extension]
                 (re-matches
                  #"^.*\.(html|ttl|json|tsv)$"
                  (string/lower-case (or uri "")))]
        extension)
      (let [accept (string/split (get headers "accept" "") #", ?")]
        (first (drop-while nil? (map #(get mimetype-table (first (string/split % #";"))) accept))))))

(defn req->content-type
  [req]
  (if-let [format (req->output-format req)]
    (format->content-type format)))

(defn with-content-header
  [f]
  (fn [req]
    (assoc-in
     (f req) [:headers "Content-Type"]
     (str (req->content-type req) "; charset=utf-8"))))
