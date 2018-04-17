(ns com.knocean.knode.pages.linked-data-fragments.core
  (:require [com.knocean.knode.pages.mimetypes :as mime]
            
            [com.knocean.knode.state :refer [state] :as st]
            [com.knocean.knode.pages.html :refer [html]]
            [com.knocean.knode.linked-data-fragments :as ldf]))

(defn slurps
  [resource]
  (let [s (java.io.PushbackReader. (clojure.java.io/reader (clojure.java.io/resource resource)))]
    (loop [ln (clojure.edn/read {:eof nil} s)
           lines []]
      (if (not (nil? ln))
        (recur (clojure.edn/read {:eof nil} s) (conj lines ln))
        lines))))

(defmulti ldf-result mime/req->output-format)

(defmethod ldf-result "html"
  [{:keys [ldf-query session] :as req}]
  (html
   {:session session
    :title "Linked Data Fragments Result"
    :content (let [res (ldf/query ldf-query (slurps "obi_core.edn"))
                   ix (* (:page res) (:per-page res))]
               (if (empty? (:items res))
                 [:span "Not enough results..."]
                 [:span
                  [:p "Showing results " (inc ix) " to " (+ ix (count (:items res))) " of " (:total res) " with " (:per-page res) " per page."]
                  [:ul (map (fn [entry] [:li (str entry)]) (:items res))]]))}))

(defn with-ldf-query
  [f]
  (fn [req]
    (f (assoc req :ldf-query (ldf/req->query req)))))

(def routes
  [["/ldf" (with-ldf-query ldf-result)]])
