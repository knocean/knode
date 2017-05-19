(ns knode.upstream
  (:require
   [org.httpkit.client :as http]
   [clojure.data.xml :as xml]))

;; (xml/parse (java.io.StringReader. (:body @(http/get "https://raw.githubusercontent.com/IEDB/MRO/v2016-12-15/mro.owl"))))

(defn xml-string->version-iri [string]
  (->> string
       java.io.StringReader. xml/parse
       :content first :content (filter #(= :versionIRI (:tag %)))
       first :attrs :rdf/resource))

(defn path-join [strings]
  (string/join "/" strings))

(defn fetch-upstream [iri]
  (http/get
   iri
   (fn [{:keys [status headers body error]}]
     (if (= status 200)
       (let [version-iri (xml-string->version-iri body)
             fname (path-join (cons "tmp" (drop 3 (string/split version-iri #"/"))))]
         (with-open [*out* (java.util.zip.GZIPInputStream. (clojure.java.io/input-stream fname))]
           (println body)))
       [:TODO "Log this somewhere" status error]))))
