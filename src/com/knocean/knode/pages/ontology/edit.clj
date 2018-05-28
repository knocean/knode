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

;; (valid-knotation? ": kn:SUBJECT\nknp:apply-template: protein class\n taxon: kn:REQUIRED\n label: kn:REQUIRED")
;; (validate-knotation ": kn:SUBJECT\nknp:apply-template: protein class\n taxon: kn:REQUIRED\n label: kn:REQUIRED")

;; TODO - these functions (and probably the template validation stuff)
;;        should all be moved to knotation-cljc
(defn validate-knotation
  ([snippet] (validate-knotation (st/latest-env) snippet))
  ([env snippet]
   (try
     (filter identity (api/errors-of (api/read-string :kn env snippet)))
     (catch Exception e
       (list [:bad-parse (.getMessage e)])))))

(defn valid-knotation?
  ([snippet] (valid-knotation? (st/latest-env) snippet))
  ([env snippet]
   (empty? (validate-knotation env snippet))))

(defn add-term
  [{:keys [env params session] :as req}]
  (if-let [raw (get params "template-text")]
    (do
      (when (and (valid-knotation? raw) (auth/logged-in? req))
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

(defn validate-term
  [{:keys [env params session] :as req}]
  (if-let [raw (string/replace (get params "template-text") #"\r\n" "\n")]
    (html
     {:session session
      :title "Validate Term"
      :content
      (let [valid? (valid-knotation? env raw)
            validated (validate-knotation env raw)]
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
          (if (empty? validated)
            [:p "Valid"]
            [:span
             [:h4 "Errors:"]
             [:ul
              (map (fn [[k v]] [:li (name k) " :: " v])
                   validated)]])]])})
    (util/redirect "/ontology")))
