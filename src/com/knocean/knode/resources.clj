(ns com.knocean.knode.resources
  (:require [clojure.java.io :as io]
            [clojure.string :as s]
            [clojure.pprint :as pp]
            [clojure.java.jdbc :as jdbc]
            [clj-jgit.porcelain :as git]

            [org.knotation.clj-api :as kn]

            [com.knocean.knode.state :as st]
            [com.knocean.knode.util :as util]))

(def types
  [:obo-published-owl :obo-github-repo :direct-link :local-file])

(def obo "http://purl.obolibrary.org/obo/")

(defn insert-branches!
  "given an :obo-github-repo resource, add each of the branched resources to the
   'resources' table. If a resource with the same label (ID) already exists, 
   remove it and add the new one."
  [{:keys [:paths :idspace :label :title :description :homepage] :as resource}]
  (doseq [p paths]
    (let [id (first (s/split p #"\."))
          res {:label id
               :title (or title id)
               :description (or description "")
               :homepage (or homepage "")}]
      (println "... branch" (last (s/split id #"-")))
      (if (empty? (st/query {:select [:*]
                             :from [:resources]
                             :where [:= :label id]}))
        (jdbc/insert! @st/state :resources res)
        (do
          (println "Old entry exists! Updating" id)
          (jdbc/delete! @st/state :resources ["label = ?" id])
          (jdbc/insert! @st/state :resources res))))))

(defn insert!
  "Given a resource entry, add the resource to the 'resources' table. If a 
   resource with the same label (idspace) already exists, delete the old
   resource entry and add the new entry."
  [{:keys [:idspace :label :title :description :homepage] :as resource}]
  (let [res {:label idspace
             :title (or title idspace)
             :description (or description "")
             :homepage (or homepage "")}]
    (if 
      (empty? 
        (st/query {:select [:*] 
                   :from [:resources] 
                   :where [:= :label idspace]}))
      (jdbc/insert! @st/state :resources res)
      (do
        (jdbc/delete! @st/state :resources ["label = ?" idspace])
        (jdbc/insert! @st/state :resources res)))))

(defn init-dir!
  "Given a string path to a directory, create the directory and any parent 
   directories if it does not exist."
  [dir]
  (let [file (io/as-file dir)]
    (if-not (.exists file)
      (do
        (io/make-parents file)
        (.mkdir file)))))

;; VALIDATION AND CHECKING

(defn is-valid?
  "Given a resource map, determine if the resource type is valid and that it has
   all required details. Return true if valid, false if not."
  [resource]
  (let [type (:type resource)
        idspace (:idspace resource)]
    (cond
      ; ID must be provided
      (nil? idspace)
      (do
        (println
          (str 
            "Cannot configure resource:\n" 
            resource 
            "\nCAUSE: resource requires an :idspace"))
        false)
      ; ID must be a string
      (not (string? idspace))
      (do
        (println
          (str
            "Cannot configure resource:\n"
            resource
            "\nCAUSE: :idspace value must be a string"))
        false)
      ; type must be a keyword
      (not (keyword? type))
      (do
        (println
          (str 
            "Cannot configure resource '" 
            idspace 
            "'\nCAUSE: resource type (" type ") must be a keyword"))
        false)
      ; type must be in the list of valid types
      (not (some #{type} types))
      (do
        (println
          (str 
            "Cannot configure resource '" 
            idspace 
            "'\nCAUSE: invalid resource type (" 
            (name type) 
            ")"))
        false)
      ; if type is :obo-github-repo, it must have the :github key
      (and
        (= type :obo-github-repo)
        (not (contains? resource :github)))
      (do
        (println
          (str 
            "Cannot configure resource '" 
            idspace 
            "'\nCAUSE: Github Repository resource must have :github key"))
        false)
      ; if the map contains :github, the :github must be a string
      (and
        (contains? resource :github)
        (not (string? (:github resource))))
      (do
        (println
          (str
            "Cannot configure resource '"
            idspace
            "'\nCAUSE: :github value must be a string"))
        false)
      ; :github value must also end with .git
      (and
        (contains? resource :github)
        (not (s/ends-with? (:github resource) ".git")))
      (do
        (println
          (str
            "Cannot configre resource '"
            idspace
            "'\nCAUSE: :github value must be a URL ending with '.git'"))
        false)
      ; otherwise, this is a valid resource
      (and
        (contains? resource :direct-link)
        (not (contains? resource :url)))
      (do
        (println
          (str
            "Cannot configure resource '"
            idspace
            "'\nCAUSE: direct link must have :url key"))
        false)
      :else true)))

(defn link-resource-exists?
  "Given a resources directory and a :direct-link resource, determine if the 
   resource has already been retrieved."
  [dir resource]
  (let [fname (-> resource
                  :idspace
                  s/lower-case
                  (str ".owl"))
        file (io/as-file (str dir "/" fname))]
    (.exists file)))

(defn exists?
  "Given a resources directory and a resource, return true if the resource does 
   exist, false otherwise. For local file resources with multiple paths, check
   that all files exist."
  [dir resource]
  (case (:type resource)
    :direct-link (link-resource-exists? dir resource)
    :obo-published-owl (link-resource-exists? dir resource)
    :obo-github-repo
    (let [fname (->> resource
                  :idspace
                  s/lower-case)
          file (io/as-file (str dir "/" fname))]
      (.exists file))
    :local-file
    (let [paths (or (:paths resource) (list (:path resource)))
          fs (map #(io/as-file %) paths)]
      (every? true? (map #(.exists %) fs)))))

;; INIT RESOURCES

(defn process-response!
  "Given a resource, a URL, an HTTP response, and a file to write to, process 
   the response and write the content to file."
  [resource url response file]
  (if-not response
    (do
      (println
        (str "Unable to fetch resource '" (:idspace resource) "' from " url)
        (System/exit 1))
      resource)
    (do
      ; first write the file to path
      (spit (.getAbsolutePath file) (slurp (last (:redirs response))))
      ; then return the (probably) updated resource map
      (if (:etag response)
        (assoc resource :etag (:etag response) :path (.getAbsolutePath file))
        (if (:last-modified response)
          (assoc resource 
            :last-modified (:last-modified response) 
            :path (.getAbsolutePath file))
          (assoc resource :path (.getAbsolutePath file)))))))

(defn get-link-resource!
  "Given a resources directory and a :direct-link resource, get the content from 
   the URL."
  [dir resource]
  (let [fname (-> resource
                  :idspace
                  s/lower-case
                  (str ".owl"))
        file (io/as-file (str dir "/" fname))
        response (util/get-response (:url resource))]
    (process-response! resource (:url resource) response file)))

(defn get-published-owl!
  "Given a resources directory and a resource of type :obo-published-owl, 
   download the resource to the directory. Return an updated resource map."
  [dir resource]
  (let [fname (-> resource
                  :idspace
                  s/lower-case
                  (str ".owl"))
        file (io/as-file (str dir "/" fname))
        url (str obo fname)
        response (util/get-response url)]
    (process-response! resource url response file)))

(defn copy-branch-resource!
  "Given a resource directory, an :obo-github-repo resource, and a branch object, 
   copy the resource file (specified by :path or an OWL file in the root of the 
   repository) from the branch to a new file in the resource directory with name 
   [id]-[branch].[ext]. Return the new file name."
  [dir {:keys [:idspace :path] :as resource} branch]
  (let [fname (->> idspace s/lower-case)
        src-file (if path
                   (str dir "/" fname "/" path)
                   (str dir "/" fname "/" fname ".owl"))
        tgt-file (str 
                   dir "/" fname "-" 
                   (-> (.getName branch) (s/split #"/") last) "." 
                   (-> src-file (s/split #"\.") last))]
    (-> src-file
        io/as-file
        (io/copy (io/as-file tgt-file)))
    (-> tgt-file (s/split #"/") last)))

(defn copy-branch-resources!
  "Given a resource directory and an :obo-github-repo resource, get the resource 
   files from each of the git branches. Return the resource with a :paths key 
   listing the path to each new resource file, copied from the repo."
  [dir resource]
  (let [rname (->> resource :idspace s/lower-case)
        repo (git/load-repo (str dir "/" rname))]
    (git/git-fetch-all repo)
    (when-let [branches (git/git-branch-list repo :all)]
      (->> branches
           (map #(copy-branch-resource! dir resource %))
           distinct
           (assoc resource :paths)))))

(defn get-github-repo!
  "Given a resources directory and a resource of type :obo-github-repo, 
   download the resource as a github repository to the directory. Copy the 
   resource files from any different branches and return the updated resource."
  [dir resource]
  (let [fname (->> resource :idspace s/lower-case)
        github (:github resource)
        repo (:repo (git/git-clone-full github (str dir "/" fname)))]
    (copy-branch-resources! dir resource)))

(defn get-resource!
  "Given a resources directory and a resource that does not exist, download 
   the resource to the directory."
  [dir resource]
  (let [type (:type resource)]
    (when (not (= type :local-file))
      (println "GET" (:idspace resource)))
    (case type
      ; error when local file does not exist
      :local-file
      (do
        (println
          (str 
            "Cannot configure resource '"
            (:idspace resource)
            "'\nCAUSE: "
            (or (:paths resource) (:path resource))
            " does not exist"))
        (System/exit 1))
      ; anything else should be fetched
      :direct-link (get-link-resource! dir resource)
      :obo-github-repo (get-github-repo! dir resource)
      :obo-published-owl (get-published-owl! dir resource))))

;; UPDATE RESOURCES

(defn different-version?
  ""
  [resource response]
  (if (:etag resource)
    ; if there is an Etag in the resource, compare it to response
    (if (= (:etag response) (:etag resource))
      ; if they are the same, do not update
      false
      true)
    ; otherwise try to find last-modified
    (if (:last-modified resource)
      ; if there is a last-modified in the resource, compare it to response
      (if (= (:last-modified response) (:last-modified resource))
        ; if they are the same, do not update
        false
        true)
      ; if there is no Etag or last-modified, resource must be updated
      true)))

(defn update-link-resource?
  ""
  [resource]
  (let [response (util/get-response (:url resource))]
    (different-version? resource response)))

(defn update-published-owl?
  "Given a resource that has the type :obo-published-owl, get a response from 
   the PURL and use the HTTP headers to determine if the file has changed."
  [resource]
  (let [fname (-> resource
                  :idspace
                  s/lower-case
                  (str ".owl"))
        url (str obo fname)
        response (util/get-response url)]
    (different-version? resource response)))

(defn update-github-repo!
  "Given a resource directory and an :obo-github-repo resource, use git-fetch to 
   update the repository then copy the resource files from each branch."
  [dir resource]
  (let [fname (->> resource :idspace s/lower-case)
        repo (git/load-repo (str dir "/" fname))]
    (git/git-fetch repo)
    (copy-branch-resources! dir resource)))

(defn maybe-update
  "Given a resources directory and an existing resource, update the resource. 
   If it is a Github repo, fetch new commits. If it is a published OWL document, 
   check the HTTP headers to see if the content has changed. Return the (maybe
   updated) resource."
  [dir resource]
  (let [type (:type resource)]
    (case type
      :local-file resource
      :obo-github-repo (update-github-repo! dir resource)
      :obo-published-owl 
      (if (update-published-owl? resource)
        (get-published-owl! dir resource)
        resource)
      :direct-link
      (if (update-link-resource? resource)
        (get-link-resource! dir resource)
        resource))))

(defn update-resource
  "Given a resources directory and a resource, configure the resource. Returns 
   the resource map, maybe updated. Return nil if the resource is not valid."
  [dir resource]
  (if (is-valid? resource)
    (if (exists? dir resource)
      (maybe-update dir resource)
      (get-resource! dir resource))
    (System/exit 1)))

(defn update-config!
  "Given a path to a configuration file and a collection of resources, write the
   (maybe updated) resources to the configuration file."
  [config resources]
  (let [file (io/as-file config)
        config-str (if (.exists file) (slurp config) nil)
        config-map (if-not (nil? config-str) (read-string config-str) nil)]
    (if-not (nil? config-map)
      (spit 
        config
        (with-out-str 
          (pp/pprint (assoc config-map :resources resources))))
      (spit 
        config
        (with-out-str (pp/pprint {:resources resources}))))))

(defn update-resources
  "Given the dereferenced application state, configure the resources for the 
   application. Skip any invalid resource configurations. Return the collection 
   of resources, maybe updated."
  [state]
  (let [resources (:resources state)
        project-dir (:absolute-dir state)
        dir (if (s/ends-with? project-dir "/")
              (str project-dir "resources")
              (str project-dir "/resources"))]
    (init-dir! dir)
    (reduce
      (fn [coll resource]
        (conj coll (update-resource dir resource)))
      []
      resources)))

;; DO EVERYTHING

(defn resources!
  "Configure the resources in a resources directory and then copy them to the 
   ontology directory in knotation format. Update the state with any changes to 
   the resources. Return a collection of the new ontology paths."
  [config]
  (let [resources (update-resources @st/state)]
    (update-config! config resources)
    (reset! st/state (assoc @st/state :resources resources))))
