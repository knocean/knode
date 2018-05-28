(ns com.knocean.knode.pages.ontology.edit
  (:require [clojure.string :as string]
            [clj-jgit.porcelain :as git]

            [org.knotation.rdf :as rdf]
            [org.knotation.environment :as en]
            [org.knotation.link :as ln]
            [org.knotation.api :as api]

            [com.knocean.knode.util :as util]
            [com.knocean.knode.state :refer [state] :as st]
            [com.knocean.knode.pages.authentication :as auth]
            [com.knocean.knode.pages.html :refer [html]]
            [com.knocean.knode.pages.ontology.base :refer [ontology-result] :as base]
            [com.knocean.knode.pages.ontology.template :as tmp]))

(defn update-state! [valid-kn]
  (let [states (org.knotation.clj-api/read-string :kn (st/latest-env) valid-kn)]
    (swap! state (fn [st] (update st :states #(concat % states))))
    nil))

(defn commit-term!
  [valid-kn user]
  (let [st @state
        id {:name (:ssh-identity st) :exclusive true}]
    (spit
     (str (:absolute-dir st) "/" (:write-file st))
     (str \newline \newline valid-kn \newline) :append true)
    (git/git-add (:repo st) (:write-file st))
    (git/git-commit
     (:repo st) "Add new term" user)
    (git/with-identity 
      (if-let [pass (:ssh-passphrase st)]
        (assoc id :passphrase pass) id)
      (git/git-push (:repo st)))
    (update-state! valid-kn)
    nil))

(defn -parse-template-application
  [env raw-template-input]
  (let [parsed (string/split-lines raw-template-input)
        [_ subject] (string/split (second parsed) #": ")
        template-iri (ln/->iri env subject)
        template (tmp/template-by-iri template-iri)
        content (string/join \newline (drop 2 parsed))]
    {:iri template-iri
     :template template
     :content content
     :content-map (tmp/parse-content content)}))

(defn add-term
  [{:keys [env params session] :as req}]
  (if-let [raw (get params "template-text")]
    (let [parsed (-parse-template-application env raw)
          valid? (tmp/valid-application? (:template parsed) (:content-map parsed))]
      (when (and valid? (auth/logged-in? req))
        (commit-term! raw (select-keys session [:name :email])))
      (html
       {:session session
        :title "Add Term"
        :content
        [:div
         [:div {:class "col-md-6"}
          [:h3 "Add Term"]
          [:script {:type "text/javascript" :src "/js/knode.js"}]
          [:textarea {:id "editor" :rows "6" :name "template-text"} raw]]
         [:div {:class "col-md-6"}
          [:h2 "Term Added!"]]]}))
    (util/redirect "/ontology/validate-term")))

;; (valid-knotation? ": SUBJECT\nknp:apply-template: protein class\n taxon: REQUIRED\n label: REQUIRED")
;; (valid-knotation? (tmp/template-dummy (tmp/template-by-iri (first (tmp/list-templates)))))

;; TODO - these functions (and probably the template validation stuff)
;;        should all be moved to knotation-cljc
(defn validate-knotation
  ([snippet] (validate-knotation (st/latest-env) snippet))
  ([env snippet]
   (try
     (let [es (filter identity (api/errors-of (api/read-string :kn env snippet)))]
       (if (empty? es)
         {}
         {:errors es}))
     (catch Exception e
       {:errors (list [:bad-parse (.getMessage e)])}))))

(defn valid-knotation?
  ([snippet] (valid-knotation? (st/latest-env) snippet))
  ([env snippet]
   (empty? (:errors (validate-knotation env snippet)))))

(defn validate-term
  [{:keys [env params session] :as req}]
  (if-let [raw (get params "template-text")]
    (html
     {:session session
      :title "Validate Term"
      :content
      (let [parsed (-parse-template-application env raw)
            valid? (and (valid-knotation? env raw)
                        (tmp/valid-application? (:template parsed) (:content-map parsed)))
            validated (merge-with
                       concat
                       (validate-knotation env raw)
                       (tmp/validate-application (:template parsed) (:content-map parsed)))]
        [:div
         [:div {:class "col-md-6"}
          [:h3 "Validate Term"]
          [:script {:type "text/javascript" :src "/js/knode.js"}]
          [:form (if valid? {:method "POST" :action "/ontology/add-term"} {:method "POST"})
           [:textarea {:id "editor" :rows "6" :name "template-text"} raw]
           [:input {:class "btn btn-primary" :type "submit"
                    :value (if valid? "Add Term" "Validate Term")}]]]
         [:div {:class "col-md-6"}
          [:h3 "Validation:"]
          (when (empty? validated)
            [:p "Valid"])
          (when (:warnings validated)
            [:span
             [:h4 "Warnings:"]
             [:ul
              (map (fn [[k v]] [:li (name k) " :: " v])
                   (:warnings validated))]])
          (when (:errors validated)
            [:span
             [:h4 "Errors:"]
             [:ul
              (map (fn [[k v]] [:li (name k) " :: " v])
                   (:errors validated))]])]] )})
    (util/redirect "/ontology")))
