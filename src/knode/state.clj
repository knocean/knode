(ns knode.state
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [environ.core :refer [env]]))

(def state
  (atom
   {:port 3210
    :root-dir ""
    :root-iri nil}))

(defn init-state!
  []
  (when (:knode-port env)
    (swap! state assoc :port (read-string (:knode-port env))))
  (when (:knode-root-dir env)
    (swap! state assoc :root-dir (:knode-root-dir env)))
  (when (:knode-root-iri env)
    (swap! state assoc :root-iri (:knode-root-iri env)))
  (if (:knode-project-name env)
    (swap! state assoc :project-name (:knode-project-name env))
    (let [path (.getAbsolutePath (io/file (:root-dir @state)))
          name (string/lower-case (.getName (io/file path)))]
      (swap! state assoc :project-name name))))

(defn testing-state!
  []
  (swap! state assoc :root-dir "test/example/")
  (swap! state assoc :root-iri "https://example.com/")
  (swap! state assoc :project-name "example"))

(init-state!)
