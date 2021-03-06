(ns com.knocean.knode.util
  (:require
   [clojure.string :as string]
   [org.httpkit.client :as http]
   [clojure.set]

   [org.knotation.environment :as en]))

(defn escape
  [iri]
  (try
    (java.net.URLEncoder/encode iri)
    (catch Exception e
      nil)))

(defn redirect [location]
  {:status 302 :headers {"Location" location}})

(defn get-response
  "Manually follows redirects and returns a vector containing all
   redirect URLs. The final URL with content is the last entry."
  [url]
  (loop [redirs []
         new-url url]
    ;; No headers from FTP, so that's final content
    (if (.contains new-url "ftp://")
      {:redirs (conj redirs new-url)}
      ;; Otherwise get HTTP status and determine what to do
      (let [{:keys [status headers]
             :as res} @(http/request {:url new-url
                                      :method :head
                                      :follow-redirects false})]
        (case status
          200
          {:redirs (conj redirs new-url)
           ;; Also get ETag and Last-Modified
           :etag (string/replace (:etag headers) #"\"" "")
           :last-modified (:last-modified headers)}
          (301 302 303 307 308)
          (recur (conj redirs new-url) (:location headers))
          nil)))))       ;; Anthing else will not be returned

(defn ->iri
  [env input]
  (or 
    (en/wrapped-iri->iri input)
    (en/curie->iri env input)
    (en/label->iri env input)
    (en/name->iri env input)))
