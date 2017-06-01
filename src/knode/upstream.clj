(ns knode.upstream
  (:require
   [clojure.java.io :as io]
   [clojure.data :as dat]
   [clojure.data.xml :as xml]

   digest
   [tempfile.core :as tmp]
   [org.httpkit.client :as http]
   [clojure.data.xml :as xml]))

(def upstream-meta
  (atom
   (if (.exists (io/file "tmp/knode-meta.edn"))
     (read-string (slurp "tmp/knode-meta.edn"))
     {})))

(defn store-upstream-meta!
  [iri meta]
  (swap! upstream-meta #(assoc % iri meta))
  (spit "tmp/knode-meta.edn" @upstream-meta)
  nil)

(defn xml->version-iri
  [stream]
  (->> stream
       xml/parse
       :content first :content (filter #(= :versionIRI (:tag %)))
       first :attrs :rdf/resource))

(defn xml-string->version-iri
  [string]
  (xml->version-iri (java.io.StringReader. string)))

(defmulti spit-gzipped! #(class %2))

(defmethod spit-gzipped! java.io.BufferedReader
  [path content]
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

(defmethod spit-gzipped! java.lang.String
  [path content]
  (spit-gzipped! path (java.io.BufferedReader. (java.io.StringReader. content))))

(defn slurp-gzipped
  [path]
  (with-open [in (java.util.zip.GZIPInputStream. (io/input-stream path))]
    (slurp in)))

(defn ->upstream-path
  [prefix iri]
  (io/as-relative-path
   (str prefix (.getPath (java.net.URL. iri)) ".gz")))

(defn iri->upstream-path
  [iri]
  (->upstream-path "tmp/" iri))

(defn iri->temp-upstream-path
  [iri]
  (->upstream-path "tmp/compare/" iri))

(defn ftp-iri?
  [iri]
  (re-find #"^ftp" iri))

(defn curl-get
  ([iri]
   (java.io.BufferedReader. (java.io.InputStreamReader. (.openStream (java.net.URL. iri)))))
  ([iri callback]
   (callback (curl-get iri))))

(defn xml->terms
  [xml-str]
  (set
   (filter
    #(not (nil? %))
    (map
     #(->> % :attrs :rdf/about)
     (->> (java.io.StringReader. xml-str) xml/parse :content)))))

(defn compare-ontologies
  [xml-str-a xml-str-b]
  (let [[in-a in-b _]
        (dat/diff (xml->terms xml-str-a) (xml->terms xml-str-b))]
    [in-a in-b]))

(defn fetch-upstream-meta!
  "Requests the given IRI from a remote server and extracts the final (non-redirect) IRI, as well as the version IRI of the corresponding XML file.
  Transparently handles HTTP re-directs to FTP resources."
  [iri]
  (if (ftp-iri? iri)
    (let [version-iri (xml->version-iri (curl-get iri))]
      {:final-iri iri :version-iri version-iri})
    (let [{:keys [status headers body error] :as res} @(http/request {:url iri :method :head :follow-redirects false})]
      (case status
        200 (let [version-iri (xml->version-iri (curl-get iri))]
              (merge
               (select-keys headers [:last-modified :etag :content-length])
               {:final-iri iri :version-iri version-iri}))
        (301 302 303 307 308) (fetch-upstream-meta! (:location headers))
        304 nil
        (throw (Exception. (str "TODO: Handle status " status)))))))

(defn get-upstream-meta!
  "Checks the cache for the given URI. If found, returns it, otherwise calls fetch-upstream-meta! to get the source."
  [uri]
  (when (not (contains? @upstream-meta uri))
    (if-let [fetched (fetch-upstream-meta! uri)]
      (store-upstream-meta! uri fetched)))
  (get @upstream-meta uri))

(defn upstream-changed?
  [iri]
  (if (ftp-iri? iri)

    (let [rdr (curl-get iri)
          version-iri (xml->version-iri rdr)
          path (iri->upstream-path version-iri)
          tmp-path (iri->temp-upstream-path version-iri)]
      (or (not (.exists (io/as-file path)))
          (do (spit-gzipped! tmp-path rdr)
              (not (= (digest/sha-256 (io/file tmp-path))
                      (digest/sha-256 (io/file path)))))))

    (let [{:keys [status headers body error] :as res} @(http/request {:url iri :follow-redirects false})]
      (case status
        200 (let [relevant (select-keys headers [:last-modified :etag :content-length])
                  cached (get @upstream-meta iri)]
              (not (and cached (= relevant cached))))
        (301 302 303 307 308) (upstream-changed? (:location headers))
        304 false
        (throw (Exception. (str "TODO: Handle status " status)))))))

(defn fetch-upstream!
  [iri]
  (if (ftp-iri? iri)
    (let [rdr (curl-get iri)
          version-iri (xml->version-iri rdr)
          fname (iri->upstream-path version-iri)]
      (spit-gzipped! fname rdr)
      (store-upstream-meta! iri {:sha256 (digest/sha-256 (io/file fname))})
      (deliver (promise) true))
    (http/request
     {:url iri
      :follow-redirects false}
     (fn [{:keys [status headers body error]}]
       (case status
         200 (let [version-iri (xml-string->version-iri body)
                   fname (iri->upstream-path version-iri)
                   relevant (select-keys headers [:last-modified :etag :content-length])]
               (store-upstream-meta! iri relevant)
               (spit-gzipped! fname body))
         (301 302 303 307 308) (fetch-upstream! (:location headers))
         (throw (Exception. (str "TODO: Handle status " status))))))))
