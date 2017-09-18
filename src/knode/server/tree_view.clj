(ns knode.server.tree-view
  (:require
   [clojure.java.io :as io]
   [clojure.edn :as edn]
   [clojure.data.json :as json]
   [clojure.string :as str]

   [knode.state :refer [state]]
   [knode.util :as util]
   [knode.server.handlers :as handlers]
   [knode.server.template :as pg]
   [knode.server.util :as sutil]))


(defn read-tree! [tree-name]
  (try
    (let [fname (str (:ontology-dir @state) "trees.edn")]
      (get (edn/read (java.io.PushbackReader. (io/reader fname))) tree-name []))
    (catch Exception e
      [])))

(defn triples->parent-lookup [triples]
  (reduce
   (fn [memo [subject parent label]]
     (assoc memo parent
            (conj
             (get memo parent [])
             {:text label :iri subject})))
   {} triples))

(defn tree->tsv2 [triples]
  (let [numbered (->> triples
                      (sort-by #(get % 2) util/natural-compare)
                      (map (fn [n trip] (cons n trip)) (range)))
        parent-tbl (reduce
                    (fn [memo [line-num subject parent label]]
                      (assoc memo subject [line-num parent]))
                    {} numbered)
        path-of (fn [iri]
                  (reverse
                   ((fn rec [k]
                      (let [[line-num parent] (get parent-tbl k)]
                        (if (nil? parent)
                          (list line-num)
                          (cons line-num (lazy-seq (rec parent))))))
                    iri)))]
    (map
     (fn [[line-num subject parent label]]
       [line-num subject parent label
        (str/join ":" (path-of subject))])
     numbered)))

(defn tree-children [triples root]
  (->> triples
       (filter
        (if (empty? root)
          (fn [[a b c]] (nil? b))
          (fn [[a b c]] (= b root))))
       (map (fn [[subject parent label]]
              {:text label :iri subject :id subject
               :children (some #(= subject (second %)) triples)}))
       (sort-by :text util/natural-compare)
       vec))

(defn render-tree-children
  [req]
  (let [format (sutil/parse-request-output-format req)]
    {:status 200
     :headers (sutil/format->content-type format)
     :body (let [tree (read-tree! (get-in req [:params "name"]))
                 root (get-in req [:params "root"])
                 children (tree-children tree root)]
             (case format
               "json" (json/write-str children)
               "edn" (str children)
               (json/write-str children)))}))
(handlers/intern-handler-fn! "/api/tree/:name/children" :render-tree-children render-tree-children)

(defn render-tree-data
  [req]
  (let [format (sutil/parse-request-output-format req)]
    {:status 200
     :headers (sutil/format->content-type format "edn")
     :body (let [tree (read-tree! (get-in req [:params "name"]))]
             (case format
               "json" (json/write-str tree)
               "edn" (str tree)
               "tsv" (sutil/seq->tsv-string tree)
               "tsv2" (sutil/seq->tsv-string (tree->tsv2 tree))
               (str tree)))}))
(handlers/intern-handler-fn! "/api/tree/:name/nodes" :render-tree-data render-tree-data)

(defn render-tree-view
  [req]
  (let [name (get-in req [:params "name"])]
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
                  (format "document.tree_name = \"%s\"" name)]]})}))
(handlers/intern-handler-fn! "/tree/:name" :render-tree-view render-tree-view)
