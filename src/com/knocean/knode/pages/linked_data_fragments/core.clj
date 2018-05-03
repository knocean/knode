(ns com.knocean.knode.pages.linked-data-fragments.core
  (:require [clojure.string :as string]
            [cemerick.url :as url]

            [org.knotation.object :as ob]
            [org.knotation.rdf :as rdf]
            [org.knotation.link :as ln]

            [com.knocean.knode.pages.mimetypes :as mime]
            
            [com.knocean.knode.state :refer [state] :as st]
            [com.knocean.knode.pages.html :refer [html]]
            [com.knocean.knode.linked-data-fragments.core :as ldf]))

(defmulti ldf-result mime/req->output-format)

(defmethod ldf-result "html"
  [{:keys [ldf-query session env] :as req}]
  (html
   {:session session
    :title "Linked Data Fragments Result"
    :content (let [res (ldf/query ldf-query (:maps @st/state))
                   ix (* (:page res) (:per-page res))]
               (if (empty? (:items res))
                 [:span "Not enough results..."]
                 [:span
                  [:p "Showing results " (inc ix) " to " (+ ix (count (:items res))) " of " (:total res) " with " (:per-page res) " per page."]
                  [:ul
                   (map
                    (fn [entry]
                      (let [obj (string/replace
                                 (string/replace
                                  (ob/object->nquads-object
                                   {::rdf/lexical (:ol entry)
                                    ::rdf/language (:ln entry)
                                    ::rdf/datatype (:di entry)})
                                  #"<" "&lt;")
                                 #">" "&gt;")]
                        [:li
                         [:a {:href (str "?subject=" (url/url-encode (:si entry)))} (ln/iri->name env (:si entry))] " "
                         [:a {:href (str "?predicate=" (url/url-encode (:pi entry)))} (ln/iri->name env (:pi entry))] " "
                         [:a {:href (str "?object=" (url/url-encode obj))} obj]]))
                    (:items res))]]))}))

(defn with-ldf-query
  [f]
  (fn [req]
    (f (assoc req :ldf-query (ldf/req->query req) :env (st/latest-env)))))

(def routes
  [["/ldf" (with-ldf-query ldf-result)]])
