(ns knode.state
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [environ.core :refer [env]]))

(def default-state
  {:port 3210
   :root-dir ""
   :root-iri nil
   :dev-key nil})

(def state (atom default-state))

(defn default-term-iri-format
  [{:keys [root-iri project-name]}]
  (str root-iri "ontology/" (string/upper-case project-name) "_%07d"))

(defn init-state!
  []
  (when (:knode-port env)
    (swap! state assoc :port (read-string (:knode-port env))))
  (when (:knode-root-dir env)
    (swap! state assoc :root-dir (:knode-root-dir env)))
  (when (:knode-root-iri env)
    (swap! state assoc :root-iri (:knode-root-iri env)))
  (when (:knode-dev-key env)
    (swap! state assoc :dev-key (:knode-dev-key env)))
  (if (:knode-project-name env)
    (swap! state assoc :project-name (:knode-project-name env))
    (let [path (.getAbsolutePath (io/file (:root-dir @state)))
          name (string/lower-case (.getName (io/file path)))]
      (swap! state assoc :project-name name)))
  (swap!
   state
   assoc
   :term-iri-format
   (if (:knode-term-iri-format env)
     (:knode-term-iri-format env)
     (default-term-iri-format @state))))

(init-state!)
