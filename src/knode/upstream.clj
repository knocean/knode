(ns knode.upstream
  (:require
   [clojure.java.io :as io]

   [org.httpkit.client :as http]
   [clojure.data.xml :as xml]))

(defn xml->version-iri [stream]
  (->> stream
       xml/parse
       :content first :content (filter #(= :versionIRI (:tag %)))
       first :attrs :rdf/resource))

(defn xml-string->version-iri [string]
  (xml->version-iri (java.io.StringReader. string)))

(defmulti spit-gzipped! #(class %2))

(defmethod spit-gzipped! java.io.BufferedReader [path content]
  (io/make-parents path)
  (with-open [s (-> path
                    clojure.java.io/output-stream
                    java.util.zip.GZIPOutputStream.
                    clojure.java.io/writer)]
    (binding [*out* s]
      (loop [ln (.readLine content)]
        (when ln
          (println ln)
          (recur (.readLine content)))))))

(defmethod spit-gzipped! java.lang.String [path content]
  (io/make-parents path)
  (with-open [s (-> path
                    clojure.java.io/output-stream
                    java.util.zip.GZIPOutputStream.
                    clojure.java.io/writer)]
    (binding [*out* s] (print content))))

(defn slurp-gzipped [path]
  (with-open [in (java.util.zip.GZIPInputStream. (io/input-stream path))]
    (slurp in)))

(defn iri->upstream-path [iri]
  (io/as-relative-path (str "tmp/" (.getPath (java.net.URL. iri)))))

(defn fetch-upstream [iri]
  (if (re-find #"^ftp" iri)
    (let [rdr (java.io.BufferedReader. (java.io.InputStreamReader. (.openStream (java.net.URL. iri))))
          version-iri (xml->version-iri rdr)
          fname (iri->upstream-path version-iri)]
      (spit-gzipped! fname rdr))
    (http/request
     {:url iri
      :follow-redirects false}
     (fn [{:keys [status headers body error]}]
       (case status
         200 (let [version-iri (xml-string->version-iri body)
                   fname (iri->upstream-path version-iri)]
               (spit-gzipped! fname body))
         (301 302 303 307 308) (fetch-upstream (:location headers))
         (throw (Exception. (str "TODO: Handle status " status))))))))
