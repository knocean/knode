(ns com.knocean.knode.pages.authentication
  (:require
   [clojure.string :as string]
   [clojure.set :as set]
   [cheshire.core :as json]
   [bidi.ring :as bring]
   [oauth.google :as google]

   [com.knocean.knode.state :refer [state]]

   [com.knocean.knode.pages.mimetypes :as mime]
   [com.knocean.knode.pages.html :refer [html]]))

(defn req->redirect-url [req local-path]
  (str (name (get req :scheme :https)) "://" (get-in req [:headers "host"]) local-path ))

(defn login-available?
  []
  (let [google-id (:google-client-id @state)
        google-secret (:google-client-secret @state)]
    (and (string? google-id)
         (not (string/blank? google-id))
         (string? google-secret)
         (not (string/blank? google-secret)))))

(defn logged-in?
  [{:keys [session params headers] :as req}]
  (or (if-let [k (get @state :api-key)]
        (or (= k (get params "api-key"))
            (= k (get headers "x-api-key"))))
      (and (not (empty? session))
           (set/subset? #{:name :email :id} (set (keys session))))))

(defn api-key-only
  [f]
  (fn [req]
    (if (get @state :write-file)
      (if (get @state :api-key)
        (if-let [api-key (or (get-in req [:headers "x-api-key"])
                             (get-in req [:params "api-key"]))]
          (if (= api-key (get @state :api-key))
            (f req)
            {:status 401
             :body "ERROR: API key not authorized"})
          {:status 403
           :body "ERROR: API key required"})
        {:status 403
         :body "ERROR: Server not configured with an API key"})
      {:status 405
       :body "ERROR: Server not configured with a write-file"})))

(defn logged-in-only
  [f]
  (fn [req]
    (if (logged-in? req)
      (f req)
      (let [fmt (mime/req->output-format req)]
        (case fmt
          "json" {:status 400
                  :headers {"Content-Type" (mime/format->content-type fmt)}
                  :body (json/encode {:error "logged-in only"})}
          "html" (html
                  {:title "Logged In Only"
                   :error "You must be logged in to see this page."}))))))

(defn json-error
  [status & messages]
  {:status status
   :headers {"Content-Type" "application/json"}
   :body (json/encode {:error (string/join " " messages)})})

(defn login
  [req]
  (let [host (get-in req [:headers "host"])
        google-id (:google-client-id @state)
        google-secret (:google-client-secret @state)]
    (html
     (if (login-available?)
       {:title "Log In"
        :content
        [:a
         {:href (oauth.google/oauth-authorization-url
                 (:google-client-id @state)
                 (req->redirect-url req "/oauth2-callback-google"))}
         "Log in with your Google account."]}
       {:title "Cannot Log In"
        :error "No login options have been configured for this project."}))))

(defn login-google
  [req]
  (let [host (get-in req [:headers "host"])
        google-id (:google-client-id @state)
        google-secret (:google-client-secret @state)]
    (cond
      (or (not (string? host)) (string/blank? host))
      (json-error 403 "The host could not be determined.")

      (or (not (string? google-id)) (string/blank? google-id))
      (json-error 403 "The Google Client ID has not been configured.")

      (or (not (string? google-secret)) (string/blank? google-secret))
      (json-error 403 "The Google Client Secret has not been configured.")

      :else
      (bring/redirect
       (google/oauth-authorization-url
        google-id
        (str "https://" host "/oauth2-callback-google"))))))

(defn oauth2-callback-google
  [req]
  (let [session
        (->> (google/oauth-access-token
              (:google-client-id @state)
              (:google-client-secret @state)
              (get-in req [:params "code"])
              (req->redirect-url req "/oauth2-callback-google"))
             :access-token
             google/oauth-client
             google/user-info)]
    (assoc
     (html
      {:session session
       :title "Logged In"
       :message "You have logged in."})
     :session session)))

(defn logout
  [req]
  (assoc
   (html
    {:title "Log Out"
     :message "You have logged out."})
   :session {}))

(def routes
  [["/login" login]
   ["/login-google" login-google]
   ["/oauth2-callback-google" oauth2-callback-google]
   ["/logout" logout]])
