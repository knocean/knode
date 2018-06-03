(ns com.knocean.knode.util
  (:require
   [clojure.string :as string]))

(defn escape
  [iri]
  (java.net.URLEncoder/encode iri))

(defn redirect [location]
  {:status 302 :headers {"Location" location}})
