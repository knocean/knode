(ns knode.upstream
  (:require
   [clojure.java.io :as io]

   [org.httpkit.client :as http]
   [clojure.data.xml :as xml]))

;; (xml/parse (java.io.StringReader. (:body @(http/get "https://raw.githubusercontent.com/IEDB/MRO/v2016-12-15/mro.owl"))))

(defn xml-string->version-iri [string]
  (->> string
       java.io.StringReader. xml/parse
       :content first :content (filter #(= :versionIRI (:tag %)))
       first :attrs :rdf/resource))

(defn spit-gzipped! [path content]
  (io/make-parents path)
  (with-open [s (-> path
                    clojure.java.io/output-stream
                    java.util.zip.GZIPOutputStream.
                    clojure.java.io/writer)]
    (binding [*out* s]
      (println content))))

(defn slurp-gzipped [path]
  (with-open [in (java.util.zip.GZIPInputStream. (io/input-stream path))]
    (slurp in)))

(defn fetch-upstream [iri]
  (http/get
   iri
   (fn [{:keys [status headers body error]}]
     (if (= status 200)
       (let [version-iri (xml-string->version-iri body)
             fname (io/as-relative-path (str "tmp/" (.getPath (java.net.URL. version-iri))))]
         (spit-gzipped fname body))
       [:TODO "Log this somewhere" status error]))))
