(ns knode.server.tree-view
  (:require
   [clojure.java.io :as io]
   [clojure.edn :as edn]
   [clojure.data.json :as json]
   [clojure.string :as str]

   [knode.state :refer [state]]
   [knode.server.template :as pg]
   [knode.server.util :as sutil]))

(defn vector-compare [[value1 & rest1] [value2 & rest2]]
  (let [result (compare value1 value2)]
    (cond
      (not (zero? result)) result
      (nil? value1) 0
      :else (recur rest1 rest2))))

(defn prepare-string [s]
  (let [s (or s "")
        parts (vec (clojure.string/split s #"\d+"))
        numbers (->> (re-seq #"\d+" s)
                     (map #(Long/parseLong %))
                     (vec))]
    (vec (interleave (conj parts "") (conj numbers "")))))

(defn natural-compare [a b]
  (vector-compare (prepare-string a) (prepare-string b)))

(defn read-tree! [tree-name]
  (try
    (let [fname (str (:ontology-dir @state) "trees.edn")]
      (get (edn/read (java.io.PushbackReader. (io/reader fname))) tree-name []))
    (catch Exception e
      [])))

(defn tree-children [triples root]
  (->> triples
       (filter
        (if (empty? root)
          (fn [[a b c]] (nil? b))
          (fn [[a b c]] (= b root))))
       (map (fn [[subject parent label]]
              {:text label :iri subject :id subject
               :children (some #(= subject (second %)) triples)}))
       (sort-by :text natural-compare)
       vec))

(defn render-tree-children
  [req name]
  (let [format (sutil/parse-request-output-format req)]
    {:status 200
     :headers (sutil/format->content-type format)
     :body (let [tree (read-tree! name)
                 root (get-in req [:params "root"])
                 children (tree-children tree root)]
             (case format
               "json" (json/write-str children)
               "edn" (str children)
               (json/write-str children)))}))

(defn render-tree-data
  [req name]
  (let [format (sutil/parse-request-output-format req)]
    {:status 200
     :headers (sutil/format->content-type format "edn")
     :body (let [tree (read-tree! name)]
             (case format
               "json" (json/write-str tree)
               "edn" (str tree)
               "tsv" (sutil/seq->tsv-string tree)
               (str tree)))}))

(defn render-tree-view
  [req name]
  {:status 200
   :headers {"Content-Type" "text/html"}
   :body
   (pg/base-template
    req
    {:title (str name " - Tree View")
     :content [:div
               [:div {:class "tree"}]
               [:link {:href "/assets/inspire-tree/inspire-tree.min.css" :rel "stylesheet"}]
               [:script {:src "/assets/inspire-tree/lodash.min.js" :type "text/javascript" :charset "utf-8"}]
               [:script {:src "/assets/inspire-tree/inspire-tree.min.js" :type "text/javascript" :charset "utf-8"}]
               [:script {:src "/assets/inspire-tree/inspire-tree-dom.min.js" :type "text/javascript" :charset "utf-8"}]
               [:script {:src "/js/tree_view.js" :type "text/javascript" :charset "utf-8"}]
               [:script {:type "text/javascript" :charset "utf-8"}
                (format "document.tree_name = \"%s\"" name)]]})})
