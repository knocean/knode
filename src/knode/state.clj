(ns knode.state
  (:require [environ.core :refer [env]]))

(def state
  (atom
   {:port (if (env :knode-port) (read-string (env :knode-port)) 3210)
    :root-dir (env :knode-root-dir)
    :root-iri (env :knode-root-iri)}))
