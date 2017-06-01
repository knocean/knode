(ns knode.server.authentication
  (:require
   [clojure.string :as string]
   [ring.util.response :refer [redirect]]
   [oauth.google :as google]

   [knode.state :refer [state]]

   [knode.server.util :as sutil]
   [knode.server.template :as pg]))

(defn login
  [req]
  (let [host (get-in req [:headers "host"])
        google-id (:google-client-id @state)
        google-secret (:google-client-secret @state)]
    {:status 200
     :headers {"Content-Type" "text/html"}
     :body
     (pg/base-template
      req
      (if (sutil/login? req)
        {:title "Log In"
         :content
         [:a
          {:href (str "https://" host "/login-google")}
          "Log in with your Google account."]}
        {:title "Cannot Log In"
         :error "No login options have been configured for this project."}))}))

(defn login-google
  [req]
  (let [host (get-in req [:headers "host"])
        google-id (:google-client-id @state)
        google-secret (:google-client-secret @state)]
    (cond
      (or (not (string? host)) (string/blank? host))
      (sutil/json-error 403 "The hosst could not be determined.")

      (or (not (string? google-id)) (string/blank? google-id))
      (sutil/json-error 403 "The Google Client ID has not been configured.")

      (or (not (string? google-secret)) (string/blank? google-secret))
      (sutil/json-error 403 "The Google Client Secret has not been configured.")

      :else
      (redirect
       (google/oauth-authorization-url
        google-id
        (str "https://" host "/oauth2-callback-google"))))))

(defn oauth2-callback-google
  [req]
  (let [session
        (->> (get-in req [:headers "host"])
             (format "https://%s/oauth2-callback-google")
             (google/oauth-access-token
              (:google-client-id @state)
              (:google-client-secret @state)
              (string/replace (:query-string req) #"^code=" ""))
             :access-token
             google/oauth-client
             google/user-info)]
    {:status 200
     :headers {"Content-Type" "text/html"}
     :session session
     :body
     (pg/base-template
      (assoc req :session session)
      {:title "Logged In"
       :message "You have logged in."})}))

(defn logout
  [req]
  {:status 200
   :headers {"Content-Type" "text/html"}
   :session {}
   :body
   (pg/base-template
    (dissoc req :session)
    {:title "Log Out"
     :message "You have logged out."})})
