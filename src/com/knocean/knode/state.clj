(ns com.knocean.knode.state
  (:require [clojure.java.io :as io]
            [clojure.string :as string]

            [environ.core :refer [env]]
            [clj-jgit.porcelain :as git]
            [me.raynes.fs :as fs]

            [honeysql.core :as sql]
            [clojure.java.jdbc :as jdbc]

            [org.knotation.state :as st]
            [org.knotation.api :as kn-api]
            [org.knotation.clj-api :as kn]
            [org.knotation.rdf :as rdf]
            [org.knotation.environment :as en]))

(defn slurps
  [resource]
  (let [s (java.io.PushbackReader. (clojure.java.io/reader (clojure.java.io/resource resource)))]
    (loop [ln (clojure.edn/read {:eof nil} s)
           lines []]
      (if (not (nil? ln))
        (recur (clojure.edn/read {:eof nil} s) (conj lines ln))
        lines))))

;; This is a set of maps describing configuration options.
;; They are in a specific order.
;; The :default key is a function from an ENV map to a default value.
(def configurators
  [{:key :project-dir
    :label "Project directory"
    :default (constantly "")}
   {:key :absolute-dir
    :label "Absolute directory"
    :default #(->> % :project-dir fs/expand-home io/file .getAbsolutePath)}
   {:key :project-name
    :label "Project name"
    :default #(->> % :absolute-dir io/file .getName)}
   {:key :base-iri
    :label "Base IRI"
    :default (constantly "https://ontology.iedb.org/ontology/ONTIE_")}
   {:key :port
    :label "Port"
    :default (constantly "3210")}
   {:key :database-url
    :label "Database URL"
    :default #(str "jdbc:sqlite:" (:absolute-dir %) "/knode.db")}
   {:key :database-type
    :label "Database type"
    :default #(-> % :database-url (string/split #":") second)}
   {:key :connection
    :label "Database connection"
    :default #(jdbc/get-connection (:database-url %))}

   {:key :git-repo
    :label "Git repository"
    :default #(if-let [r (->> % :absolute-dir git/discover-repo)]
                (git/load-repo r))}
   {:key :ssh-identity
    :label "SSH identity"
    :default (constantly "~/.ssh/id_rsa")}
   {:key :ssh-passphrase
    :label "SSH passphrase"
    :default (constantly nil)}

   {:key :api-key
    :label "API key"
    :default (constantly nil)}
   {:key :write-file
    :label "Write file"
    :default #(->> % :build-files last)}
    
   {:key :resources
    :label "Resources"
    :default (constantly nil)}])

   ; Older

   ;{:key :project-iri
   ; :label "Project IRI"
   ; :default #(str (:root-iri %) "/ontology/" (:project-name %) "_")}
   ;{:key :ssh-identity
   ; :label "SSH identity"
   ; :default (constantly "~/.ssh/id_rsa")}
   ;{:key :ssh-passphrase
   ; :label "SSH passphrase"
   ; :default (constantly nil)}
   ;{:key :readme
   ; :label "README file"
   ; :default #(io/file (str (:absolute-dir %) "/README.md"))}

   ;{:key :maps-file
   ; :label "Maps Source File"
   ; :default (constantly "obi_core.edn")}
   ;{:key :maps
   ; :label "Maps Data"
   ; :default #(slurps (:maps-file %))}

   ;{:key :google-client-id
   ; :label "Google Client ID"
   ; :default (constantly "")}
   ;{:key :google-client-secret
   ; :label "Google Client Secret"
   ; :default (constantly "")}
   ;{:key :build-files
   ; :label "Build files"
   ; :default (constantly "ontology/context.kn ontology/content.kn")}
   ;{:key :states
   ; :label "Create state"
   ; :default #(let [fs (map (fn [f] (str (:absolute-dir %) "/" f)) (string/split (:build-files %) #" "))]
   ;             (when (every? identity (map (fn [f] (.exists (io/file f))) fs))
   ;               (vec (kn/read-paths nil nil fs))))}]

(defn configure
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

(defn configure-from-file
  "Given a config file containing a map of configuration options, apply the 
   configurators (in order) and return a new map."
  [config]
  (let [file (io/as-file config)
        config-str (if (.exists file) (slurp config) nil)
        state (if-not (nil? config-str) (read-string config-str) {})]
    (reduce
      (fn [state {:keys [key label default] :as configurator}]
        (println "RUNNING CONFIGURATOR" label "...")
        (if (find state key)
          state
          (assoc state key (default state))))
      (->> configurators
           (map :key)
           (select-keys state))
      configurators)))

(defn plain-view
  [label value]
  (format "%-20s %s" (str label ":") (str value)))

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
(defonce state (atom {}))

(defn init
  "Initialize the state atom from environment variables."
  []
  (->> env
       (filter #(.startsWith (name (key %)) "knode-"))
       (map (fn [[k v]] [(-> k name (string/replace "knode-" "") keyword) v]))
       (into {})
       configure
       (reset! state)))

(defn init-from-config
  "Initialize the state atom from a configuration file."
  [& {:keys [config]
      :or {config "knode.edn"}}]
  (->> config
       configure-from-file
       (reset! state)))

(def columns [:rt :gi :si :sb :pi :oi :ob :ol :di :ln])

(defn query
  [honey]
  (let [dbspec {:connection-uri (:database-url @state)}]
    (jdbc/query dbspec (sql/format honey))))

(defn select
  [where & args]
  (query (apply sql/build :select :* :from :states :where where args)))

(defn execute!
  [query]
  (jdbc/execute! @state query))

(defn insert!
  "Given an ID space and a collection of states from a resource, add the states 
   to the 'states' table. If states with the same resource ID already exist, 
   remove them and add the new states."
  [idspace states]
  (if (empty? (query {:select [:*] :from [:states] :where [:= :rt idspace]}))
    (->> states
         (filter :pi)
         (map (apply juxt columns))
         (jdbc/insert-multi! @state :states columns))
    (do 
      (println "UPDATING STATES FOR" idspace)
      (jdbc/delete! @state :states ["rt = ?" idspace])
      (->> states
         (filter :pi)
         (map (apply juxt columns))
         (jdbc/insert-multi! @state :states columns)))))

(def -env-cache (atom nil))
(defn clear-env-cache! []
  (reset! -env-cache nil))

(defn latest-env []
  (or @-env-cache
      (reset!
       -env-cache
       (-> [:= :rt (:project-name @state)]
            select
            kn/collect-env
           (en/add-prefix "rdf" (rdf/rdf))
           (en/add-prefix "rdfs" (rdf/rdfs))
           (en/add-prefix "xsd" (rdf/xsd))
           (en/add-prefix "owl" (rdf/owl))
           (en/add-prefix "obo" "http://purl.obolibrary.org/obo/")
           (en/add-prefix "NCBITaxon" "http://purl.obolibrary.org/obo/NCBITaxon_")
           (en/add-prefix "ncbitaxon" "http://purl.obolibrary.org/obo/ncbitaxon#")
           (en/add-prefix "kn" (rdf/kn))
           (en/add-prefix "knd" (rdf/kn "datatype/"))
           (en/add-prefix "knp" (rdf/kn "predicate/"))
           (en/add-prefix (:project-name @state) (:base-iri @state))))))

(defn latest-prefix-states
  []
  (kn-api/prefix-states (latest-env)))

(defn base-env
  []
  (-> en/default-env
      (en/add-prefix "obo" "http://purl.obolibrary.org/obo/")
      (en/add-prefix "NCBITaxon" "http://purl.obolibrary.org/obo/NCBITaxon_")
      (en/add-prefix "ncbitaxon" "http://purl.obolibrary.org/obo/ncbitaxon#")
      (en/add-prefix (:project-name @state) (:base-iri @state))))

(defn build-env
  [iris]
  (if (first iris)
    (->> {:select [:si :ol] :modifiers [:distinct] :from [:states]
          :where [:and
                  [:= :pi "http://www.w3.org/2000/01/rdf-schema#label"]
                  [:in :si (remove nil? iris)]]}
         query
         (reduce
          (fn [env row] (en/add-label env (:ol row) (:si row)))
          (base-env)))
    (base-env)))

(defn build-env-from-states
  [states]
  (->> states
       (mapcat (juxt :si :pi :oi))
       (remove nil?)
       set
       build-env))
