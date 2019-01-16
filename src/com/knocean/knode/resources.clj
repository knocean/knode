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

(defn insert!
  "Given a resource entry, add the resource to the 'resources' table. If a 
   resource with the same label (idspace) already exists, remove it and add the 
   new one."
  [resource]
  (let [res {:label (:idspace resource)
             :title (or (:title resource) (:idspace resource))
             :description (or (:description resource) "")
             :homepage (or (:homepage resource) "")}]
    ; query to see if resource with same label exists, delete if so
    (if (empty? (st/query {:select [:*] 
                           :from [:resources] 
                           :where [:= :label (:label res)]}))
      (jdbc/insert! @st/state :resources res)
      (do
        (println "UPDATING" (:label res))
        (jdbc/delete! @st/state :resources ["label = ?" (:label res)])
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
  ""
  [dir resource]
  (let [fname (-> resource
                  :idspace
                  s/lower-case
                  (str ".owl"))
        file (io/as-file (str dir "/" fname))]
    (.exists file)))

(defn exists?
  "Given a resources directory and a resource, return true if the resource does 
   exist, false otherwise."
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
    (let [file (io/as-file (:path resource))]
      (if (.exists file)
        true
        false))))

;; INIT RESOURCES

(defn process-response!
  ""
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
        (assoc resource :etag (:etag response))
        (if (:last-modified response)
          (assoc resource :last-modified (:last-modified response))
          resource)))))

(defn get-link-resource!
  ""
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

(defn get-github-repo!
  "Given a resources directory and a resource of type :obo-github-repo, 
   download the resource as a github repository to the directory."
  [dir resource]
  (let [fname (->> resource :idspace s/lower-case)
        github (:github resource)
        repo (:repo (git/git-clone-full github (str dir "/" fname)))]
    resource))

(defn get-resource!
  "Given a resources directory and a resource that does not exist, download 
   the resource to the directory."
  [dir resource]
  (let [type (:type resource)]
    (case type
      ; error when local file does not exist
      :local-file
      (do
        (println
          (str 
            "Cannot configure resource '"
            (:idspace resource)
            "'\nCAUSE: "
            (:path resource)
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
  ""
  [dir resource]
  (let [fname (->> resource :idspace s/lower-case)
        repo (git/load-repo (str dir "/" fname))]
    (git/git-fetch repo)
    resource))

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

;; MAIN CONFIG METHODS

(defn update-resources-and-config
  "Given the dereferenced application state and the path to the configuration 
   file, configure the resources for the application and write any updates to 
   the configuration file. Return the state with any updates to the resources."
  [state config]
  (let [resources (update-resources state)]
    (update-config! config resources)
    (assoc state :resources resources)))

;; MOVE RESOURCES TO ONTOLOGY DIRECTORY

(defn switch-branch!
  "Given a resource directory and a resource of type :obo-github-repo,
   maybe switch the branch determined by the :branch key in the resource.
   If :branch is not provided, stay on master branch."
  [dir resource]
  (let [fname (->> resource :idspace s/lower-case)
        repo (git/load-repo (str dir "/" fname))
        branch (:branch resource)]
    (if-not (nil? branch)
      (do
        (println "Fetching all refs...")
        (git/git-fetch-all repo)
        (println "Switching to branch" branch)
        (git/git-checkout repo branch)))))

(defn copy-file!
  "Given a source file and a target file (in KN format),
   copy the source file to the target file in knotation format. 
   Return the path to the new file on success, nil on failure."
  [src-file tgt-file]
  (let [out-file (io/as-file tgt-file)]
    (if (s/ends-with? src-file ".kn")
      ; if the src file is already in knotation,
      ; just copy it to the destination
      (-> src-file 
          (io/as-file)
          (io/copy out-file))
      ; otherwise, we need to convert to kn
      (kn/render-path :kn nil (kn/read-path nil nil src-file) out-file))
    (if-not (.exists out-file)
      ; if the file does not exist after copying/converting,
      ; throw an error
      (do
        (println (str "ERROR: Unable to copy " src-file " to " tgt-file))
        (System/exit 1))
      ; otherwise, return the path to the target file
      tgt-file)))

; Assumes there is a published '.owl' file in the root of the github repo
; TODO: detect where the file is? Allow a config file?
(defn copy-resource
  "Given a resource, a source directory to copy from, and a target directory to 
   copy to, copy the resource to the target in kn format. Return the path to the
   new file on success, nil on failure."
  [resource src-dir tgt-dir]
  (let [fname (->> resource :idspace s/lower-case)
        tgt-file (str tgt-dir "/" fname ".kn")]
    (println (str "Copying resource '" (:idspace resource) "'..."))
    (case (:type resource)
      :local-file
      (copy-file! (:path resource) tgt-file)
      :obo-github-repo 
      (do
        (switch-branch! src-dir resource)
        (copy-file! (str src-dir "/" fname "/" fname ".owl") tgt-file))
      :obo-published-owl 
      (copy-file! (str src-dir "/" fname ".owl") tgt-file)
      :direct-link
      (copy-file! (str src-dir "/" fname ".owl") tgt-file))))

(defn copy-resources
  "Given the dereferenced application state, convert any ontology files in the 
   resources directory to knotation and move them to the ontology directory. 
   Return a collection of the paths to the new files (if they were successfully
   copied)."
  [state]
  (let [resources (:resources state)
        project-dir (if (s/ends-with? (:absolute-dir state) "/")
                      (:absolute-dir state)
                      (str (:absolute-dir state) "/"))
        src-dir (str project-dir "resources")
        tgt-dir (str project-dir "ontology")]
    (init-dir! tgt-dir)
    (remove nil?
      (reduce
        (fn [coll resource]
          (conj coll (copy-resource resource src-dir tgt-dir)))
        []
        resources))))

;; DO EVERYTHING

(defn resources!
  "Configure the resources in a resources directory and then copy them to the 
   ontology directory in knotation format. Update the state with any changes to 
   the resources. Return a collection of the new ontology paths."
  [config]
  (reset! st/state (update-resources-and-config @st/state config))
  (copy-resources @st/state))
