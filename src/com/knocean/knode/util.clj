(ns com.knocean.knode.util
  (:require
   [clojure.string :as string]))

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
   (get (clojure.set/map-invert mimetype-table) format default)))

(defn req->output-format
  [{:keys [params uri headers] :as req}]
  (let [accept (string/split (get headers "accept" "") #", ?")]
    (or (when (find params "output-format")
          (string/lower-case (get params "output-format")))
        (when (find params "format")
          (string/lower-case (get params "format")))
        (when-let [[_ extension]
                   (re-matches
                    #"^.*\.(html|ttl|json|tsv)$"
                    (string/lower-case (or uri "")))]
          extension)
        (first (drop-while nil? (map #(get mimetype-table %) accept))))))

(defn req->content-type [req]
  (if-let [format (req->output-format req)]
    (format->content-type format)))

(defn redirect [location]
  {:status 302 :headers {"Location" location}})
