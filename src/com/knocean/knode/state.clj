(ns com.knocean.knode.state
  (:require [clojure.java.io :as io]
            [clojure.string :as string]

            [environ.core :refer [env]]
            [clj-jgit.porcelain :as git]
            [me.raynes.fs :as fs]

            [org.knotation.clj-api :as kn]
            [org.knotation.environment :as en]))

(defn slurps
  [resource]
  (let [s (java.io.PushbackReader. (clojure.java.io/reader (clojure.java.io/resource resource)))]
    (loop [ln (clojure.edn/read {:eof nil} s)
           lines []]
      (if (not (nil? ln))
        (recur (clojure.edn/read {:eof nil} s) (conj lines ln))
        lines))))

; This is a set of map describing configuration options.
; They are in a specific order.
; The :default key is a function from an ENV map to a default value.
(def configurators
  [{:key :root-dir
    :label "Root directory"
    :default (constantly "~/projects/knode-example")}
   {:key :port
    :label "Port"
    :default (constantly "3210")}
   {:key :root-iri
    :label "Root IRI"
    :default #(str "http://localhost:" (:port %) "/")}
   {:key :absolute-dir
    :label "Absolute directory"
    :default #(->> % :root-dir fs/expand-home io/file .getAbsolutePath)}
   {:key :repo
    :label "Git repository"
    :default #(if-let [r (->> % :absolute-dir git/discover-repo)]
                (git/load-repo r))}
   {:key :project-name
    :label "Project name"
    :default #(->> % :absolute-dir
                   io/file
                   .getName
                   string/lower-case)}
   {:key :project-iri
    :label "Project IRI"
    :default #(str (:root-iri %) "/ontology/" (:project-name %) "_")}
   {:key :readme
    :label "README file"
    :default #(io/file (str (:absolute-dir %) "/README.md"))}
   {:key :idspace
    :label "IDSPACE"
    :default #(string/upper-case (:project-name %))}
   {:key :ssh-identity
    :label "SSH identity"
    :default (constantly "~/.ssh/id_rsa")}
   {:key :ssh-passphrase
    :label "SSH passphrase"
    :default (constantly nil)}

   {:key :maps-file
    :label "Maps Source File"
    :default (constantly "obi_core.edn")}
   {:key :maps
    :label "Maps Data"
    :default #(slurps (:maps-file %))}

   {:key :google-client-id
    :label "Google Client ID"
    :default (constantly "")}
   {:key :google-client-secret
    :label "Google Client Secret"
    :default (constantly "")}
   {:key :build-files
    :label "Build files"
    :default (constantly "ontology/context.kn ontology/content.kn")}
   {:key :write-file
    :label "Write file"
    :default #(->> % :build-files last)}
   {:key :states
    :label "Create state"
    :default #(let [fs (map (fn [f] (str (:absolute-dir %) "/" f)) (string/split (:build-files %) #" "))]
                (when (every? identity (map (fn [f] (.exists (io/file f))) fs))
                  (vec (kn/read-paths nil nil fs))))}
   {:key :grouped
    :label "Group states by subject"
    :default #(group-by :si (:states %))}])

(defn init
  "Given a base map, apply the configurators (in order)
   and return a new map."
  [base]
  (reduce
   (fn [state {:keys [key label default] :as configurator}]
     (println "RUNNING CONFIGURATOR" label "...")
     (if (find state key)
       state
       (assoc state key (default state))))
   (->> configurators
        (map :key)
        (select-keys base))
   configurators))

(defn plain-view
  [label value]
  (format "%-18s %s" label (str value)))

(defn report
  "Given a state map (dereferenced),
   return a string describing the configuration options."
  [state]
  (->> configurators
       (filter :label)
       (map
        (fn [{:keys [key label default view] :or {view plain-view}}]
          (view label (get state key))))
       (string/join "\n")))

; Initialize the application state in an atom
; starting with the ENV.
(defonce state
  (-> env
      init
      atom))

(defn latest-env []
  (->> @state :states last ::en/env))
