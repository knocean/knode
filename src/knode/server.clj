(ns knode.server
  (:require
   [clojure.java.io :as io]
   [clojure.string :as string]
   [clojure.data.json :as json]
   [clojure.data.csv :as csv]

   [org.httpkit.server :as httpkit]
   [compojure.route :as route]
   [ring.util.response :refer [redirect]]
   [ring.middleware.session :refer [wrap-session]]

   [hiccup.core :as hiccup]
   [hiccup.page :as pg]
   [markdown.core :as md]
   [yaml.core :as yaml]
   [oauth.google :as google]

   [knode.state :refer [state]]
   [knode.core :as core]
   [knode.emit :as emit]
   [knode.sparql :as sparql]
   [knode.util :as util])
  (:use [compojure.core :only [defroutes GET POST]]))

(defn json-error
  [status & messages]
  {:status status
   :headers {"Content-Type" "application/json"}
   :body
   (json/write-str
    {:error (string/join " " messages)}
    :escape-slash false)})

(defn parsed-params
  [uri-param-string]
  (->> (string/split (str uri-param-string) #"[&=]")
       (map #(java.net.URLDecoder/decode %))
       (partition 2)
       (map vec)
       (into {})))

(defn login?
  [req]
  (let [host (get-in req [:headers "host"])
        google-id (:google-client-id @state)
        google-secret (:google-client-secret @state)]
    (and (string? host)
         (string? google-id)
         (not (string/blank? google-id))
         (string? google-secret)
         (not (string/blank? google-secret)))))

(defn developer?
  [req]
  (and (get-in req [:session :email])
       (not (string/blank? (:developers @state)))
       (contains?
        (set (string/split (:developers @state) #"\s+"))
        (get-in req [:session :email]))))

(defn stylesheet
  [name]
  [:link {:href (str "/assets/" name) :rel "stylesheet"}])

(defn base-template
  [req title content]
  (pg/html5
   [:head
    [:meta {:charset "utf-8"}]
    [:meta {:http-equiv "X-UA-Compatible" :content "IE=edge"}]
    [:meta {:name "viewport" :content "width=device-width, initial-scale=1"}]
    [:meta {:name "description" :content ""}]
    [:meta {:name "author" :content ""}]

    [:title title]

    (map
     stylesheet
     ["bootstrap.min.css"
      "ie10-viewport-bug-workaround.css"
      "style.css"])

    "<!--[if lt IE 9]>" [:script "/assets/ie8-responsive-file-warning.js"] "<![endif]-->"
    [:script {:src "/assets/ie-emulation-modes-warning.js"}]

    ;; HTML5 shim and Respond.js for IE8 support of HTML5 elements and media queries
    "<!--[if lt IE 9]>"
    [:script {:src "https://oss.maxcdn.com/html5shiv/3.7.3/html5shiv.min.js"}]
    [:script {:src "https://oss.maxcdn.com/respond/1.4.2/respond.min.js"}]
    "<![endif]-->"]

   [:body
    [:div {:class "container"}
     [:nav {:class "navbar navbar-default"}
      [:div {:class "container-fluid"}
       [:div {:class "navbar-header"}
        [:button {:type "button" :class "navbar-toggle collapsed" :data-toggle "collapse" :data-target "#navbar" :aria-expanded "false" :aria-controls "navbar"}
         [:span {:class "sr-only"} "Toggle navigation"]
         [:span {:class "icon-bar"}]
         [:span {:class "icon-bar"}]
         [:span {:class "icon-bar"}]]
        [:a {:class "navbar-brand" :href "/"} (:idspace @state)]]
       [:div {:id "navbar" :class "navbar-collapse collapse"}
        [:ul {:class "nav navbar-nav"}
         [:li [:a {:href (str "/ontology/" (:idspace @state))} "Terms"]]]
        (if (login? req)
          [:ul {:class "nav navbar-nav navbar-right"}
           (if-let [name (get-in req [:session :name])]
             [:li [:a {:href "/logout"} name  " (Log out)"]]
             [:li [:a {:href "/login-google"} "Log in"]])])]]]
     [:div {:id "content"} content]]

    [:script {:src "/assets/jquery.min.js"}]
    [:script {:src "/assets/bootstrap.min.js"}]
    [:script {:src "/assets/ie10-viewport-bug-workaround.js"}]]))

(defn render-status
  [req]
  {:status 200
   :headers {"Content-Type" "text/html"}
   :body
   (let [s @state
         term-count (count (:terms s))]
     (base-template
      req
      (cond (= 0 term-count) "Empty terms"
            :else "All systems go")
      [:ul [:li [:b "Terms Count"] "-" term-count]]))})

(defn render-html
  "Given a request, try to find a matching term,
   and return a response map for HTML."
  [req iris]
  (let [host (str "https://" (get-in req [:headers "host"]) "/")
        root (:root-iri @state)
        path (get-in req [:route-params :*])
        iri (when (= 1 (count iris)) (first iris))
        term (when iri (get-in @state [:terms iri]))
        subject (:subject term)
        label (get-in @state [:env :iri-labels (:iri subject)])]
    (cond
      term
      {:status 200
       :headers {"Content-Type" "text/html"}
       :body
       (base-template
        req
        label
        [:div
         [:h2 (:curie subject) " " label]
         [:p [:a {:href iri} iri]]
         (doall
          (emit/emit-rdfa-term
           (:env @state)
           (:context @state)
           (:subject term)
           (:blocks term)))
         [:p
          "Other formats: "
          [:a
           {:href (str (string/replace path #".html^" "") ".ttl")}
           "Turtle (ttl)"]
          ", "
          [:a
           {:href (str (string/replace path #".html^" "") ".json")}
           "JSON-LD (json)"]
          "."]])}

      iris
      {:status 200
       :headers {"Content-Type" "text/html"}
       :body
       (base-template
        req
        (:idspace @state)
        [:ul
         (for [iri iris]
           (let [term (get-in @state [:terms iri])
                 subject (:subject term)
                 iri (:iri subject)
                 values (core/collect-values (:blocks term))]
             [:li
              [:a
               {:href (string/replace iri root host)}
               (core/get-curie (:env @state) iri)
               " "
               (get-in values [emit/rdfs:label 0 :lexical])]]))])}

      :else
      nil)))

(defn render-ttl
  "Given a request, try to find a matching term,
   and return a response map for Turtle."
  [req iris]
  (cond
    (= 1 (count iris))
    (let [term (get-in @state [:terms (first iris)])]
      {:status 200
       :headers {"Content-Type" "text/turtle"}
       :body
       (emit/emit-ttl-term
        (:env @state)
        (:context @state)
        (:subject term)
        (:blocks term))})

    iris
    {:status 200
     :headers {"Content-Type" "text/turtle"}
     :body
     (emit/emit-ttl-terms
      (:env @state)
      (:context @state)
      (for [iri iris]
        (get-in @state [:terms iri])))}

    :else
    nil))

(defn render-jsonld
  "Given a request, try to find a matching term,
   and return a response map for JSON-LD."
  [req iris]
  (cond
    (= 1 (count iris))
    (let [term (get-in @state [:terms (first iris)])]
      {:status 200
       :headers {"Content-Type" "application/json"}
       :body
       (json/write-str
        (emit/emit-jsonld-term
         (:env @state)
         (:context @state)
         (:subject term)
         (:blocks term))
        :escape-slash false)})

    ; TODO: multiple terms

    :else
    nil))

(defn seq->tsv-string
  [headers rows]
  (with-out-str
    (csv/write-csv
     *out*
     (concat [headers] rows)
     :separator \tab)))

(defn render-tsv
  [req iris]
  (let [params (parsed-params (:query-string req))
        compact (= "true" (get params "compact"))
        select (get params "select" "CURIE,label,obsolete,replacement")
        predicate-labels (string/split select  #",")
        predicate-iris
        (map #(if (contains? #{"IRI" "CURIE"} %)
                %
                (try
                  (:iri (core/resolve-name (:env @state) {:label %}))
                  (catch Exception e nil)))
             predicate-labels)]
    (cond
      (empty? predicate-labels)
      (json-error 400 "Some predicates must be selected")

      (empty? iris)
      (json-error 400 "Some terms must be submitted")

      (not (util/all? predicate-iris))
      (json-error 400 "Some predicate labels cannot be resolved")

      :else
      {:status 200
       :headers {"Content-Type" "text/tab-separated-values"}
       :body
       (seq->tsv-string
        predicate-labels
        (sparql/query-predicates-tabular
         @state compact iris predicate-iris))})))

(defn render-doc
  [req doc]
  {:status 200
   :headers {"Content-Type" "text/html"}
   :body
   (let [path (str (:root-dir @state) "doc/" doc ".md")
         file (io/file path)]
     (when (.exists file)
       (let [[_ _ header content] (re-find #"(?s)(---(.*?)---)(.*)" (slurp file))
             metadata (yaml/parse-string header)]
         (base-template
          req
          (:title metadata)
          (md/md-to-html-string content)))))})

(defn get-next-iri
  [iri-format iris]
  (->> (range 1 10000000)
       (map (partial format iri-format))
       (remove (set iris))
       first))

(defn make-term
  [template data]
  (let [iri (get-next-iri
             (:term-iri-format @state)
             (keys (:terms @state)))
        curie (core/get-curie (:env @state) iri)
        optional-predicates
        (->> data
             keys
             (remove #{"api-key" "template"})
             (remove (set (:required-predicates template))))
        optional-pairs
        (reduce
         (fn [pairs pred]
           (let [value (get data pred)
                 values (if (sequential? value) value [value])]
             (concat pairs (map (fn [v] [pred v]) values))))
         []
         optional-predicates)]
    {:subject {:iri iri :curie curie}
     :blocks
     (->> (concat
           [{:predicate {:label "template"}
             :object {:iri (:iri template)}}]
           (for [required (:required-predicates template)]
             {:predicate {:label required}
              :content (get data required)})
           (for [[predicate content] optional-pairs]
             {:predicate {:label predicate}
              :content content}))
          (map (partial core/resolve-block (:env @state)))
          (map (partial core/resolve-content (:env @state)))
          (core/expand-templates (:env @state) (:templates @state)))}))

(defn add-term-to-state!
  [term]
  (let [iri (get-in term [:subject :iri])]
    (swap! state assoc-in [:terms iri] term)
    (spit
     (str (:root-dir @state) "ontology/index.tsv")
     (emit/emit-index (:env @state) (:terms @state)))
    (spit
     (str (:root-dir @state) "ontology/" (:project-name @state) ".kn")
     (emit/emit-kn-terms (:env @state) nil (:terms @state)))))

(defn add-valid-term!
  [template data]
  (sparql/load-terms! @state)
  (if-let [failure (sparql/validation-failure @state)]
    (json-error
     400
     "Cannot add term because ontology is currently invalid:"
     (:label failure))
    (let [term (make-term template data)
          iri (get-in term [:subject :iri])
          test-state (assoc-in @state [:terms iri] term)
          _ (sparql/load-terms! test-state)
          failure (sparql/validation-failure test-state)]
      (if failure
        (json-error 400 "Validation failed:" (:label failure))
        (do
          (add-term-to-state! term)
          {:status 201
           :headers {"Content-Type" "application/json"}
           :body
           (json/write-str
            (emit/emit-jsonld-term
             (:env @state)
             (:context @state)
             (:subject term)
             (:blocks term))
            :escape-slash false)})))))

(defn add-term!
  [data]
  (let [dev-key (:dev-key @state)
        api-key (get data "api-key")
        template-label (get data "template")
        template-iri (get-in @state [:env :labels template-label :iri])
        template (get-in @state [:templates template-iri])]
    (cond
      (not (map? data))
      (json-error 401 "Data must be submitted as a JSON object.")

      (or (not (string? dev-key)) (string/blank? dev-key))
      (json-error 403 "The developer API has not been configured.")

      (nil? api-key)
      (json-error 403 "A valid 'api-key' must be provided.")

      (not= dev-key api-key)
      (json-error 403 "The 'api-key' is not valid.")

      (nil? template-label)
      (json-error 400 "The 'template' key must be provided.")

      (nil? template-label)
      (json-error 400 "The 'template' key must be provided.")

      (nil? template)
      (json-error 400 (format "Unknown template '%s'" template-label))

      (not (every? data (:required-predicates template)))
      (json-error 400 "Required keys: " (:required-predicates template))

      :else
      (try
        (add-valid-term! template data)
        (catch Exception e
          (json-error 400 (.getMessage e)))))))

(defn add-term-json!
  [req]
  (add-term! (->> req :body slurp json/read-str)))

(defn login
  [req]
  (let [host (get-in req [:headers "host"])
        google-id (:google-client-id @state)
        google-secret (:google-client-secret @state)]
    {:status 200
     :headers {"Content-Type" "text/html"}
     :body
     (if (login? req)
       (base-template
        req
        "Log In"
        [:a
         {:href (str "https://" host "/login-google")}
         "Log in with your Google account."])
       (base-template
        req
        "Cannot Log In"
        [:p "No login options have been configured for this project."]))}))

(defn login-google
  [req]
  (let [host (get-in req [:headers "host"])
        google-id (:google-client-id @state)
        google-secret (:google-client-secret @state)]
    (cond
      (or (not (string? host)) (string/blank? host))
      (json-error 403 "The hosst could not be determined.")

      (or (not (string? google-id)) (string/blank? google-id))
      (json-error 403 "The Google Client ID has not been configured.")

      (or (not (string? google-secret)) (string/blank? google-secret))
      (json-error 403 "The Google Client Secret has not been configured.")

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
     (base-template
      (assoc req :session session)
      "Logged In"
      [:p "You have logged in."])}))

(defn logout
  [req]
  {:status 200
   :headers {"Content-Type" "text/html"}
   :session {}
   :body
   (base-template
    (dissoc req :session)
    "Log Out"
    [:p "You have logged out."])})

(defn parse-ontology-request
  "Given a state and a request map,
   return a map with :format and :iris keys."
  [state req]
  (let [params (parsed-params (:query-string req))
        body (cond
               (nil? (:body req)) nil
               (string? (:body req)) (:body req)
               :else (-> req :body slurp))]
    {:format
     (or
      (when (find params "format")
        (string/lower-case (get params "format")))
      (when-let [[_ extension]
                 (re-matches
                  #"^.*\.(html|ttl|json|tsv)$"
                  (string/lower-case
                   (get-in req [:route-params :*] "")))]
        extension))

     ; TODO: limit, offset
     :iris
     (or
      (when-let [[_ id] (re-matches
                         #"(\w+_\d+).*$"
                         (get-in req [:route-params :*] ""))]
        [(str (:root-iri state) "ontology/" id)])
      (when (find params "iris")
        (string/split (get params "iris") #"\s+"))
      (when (find params "curies")
        (->> (string/split (get params "curies") #"\s+")
             (map #(try (core/resolve-curie (:env state) {:curie %})
                        (catch Exception e {:iri %})))
             (map :iri)))
      (when body
        (let [[header & rows] (string/split-lines body)]
          (cond
            (= header "CURIE")
            (->> rows
                 (map #(try (core/resolve-curie (:env state) {:curie %})
                            (catch Exception e {:iri %})))
                 (map :iri))
            (= header "IRI")
            rows
            :else
            nil)))
      (when (= (:idspace state) (get-in req [:route-params :*]))
        (->> state :terms keys sort)))}))

(defn ontology-get
  [req]
  (let [{:keys [format iris]} (parse-ontology-request @state req)]
    (cond
      (nil? iris)
      nil

      (nil? format)
      (render-html req iris)

      (not (contains? #{"html" "ttl" "json" "tsv"} format))
      nil

      (= format "html")
      (render-html req iris)

      (= format "ttl")
      (render-ttl req iris)

      (= format "json")
      (render-jsonld req iris)

      (= format "tsv")
      (render-tsv req iris)

      :else
      nil)))

(defn ontology-post
  [req]
  (let [{:keys [format iris]} (parse-ontology-request @state req)]
    (cond
      ; default operation is to add a term
      (not= "get" (get (parsed-params (:query-string req)) "method"))
      (add-term-json! req)

      (nil? format)
      (render-html req iris)

      (not (contains? #{"html" "ttl" "json" "tsv"} format))
      (str "Unrecognized format: " format)

      (= format "html")
      (render-html req iris)

      (= format "ttl")
      (render-ttl req iris)

      (= format "json")
      (render-jsonld req iris)

      (= format "tsv")
      (render-tsv req iris)

      :else
      nil)))

(defroutes knode-routes
  ; ## Authentication
  (GET "/login" [] login)
  (GET "/login-google" [] login-google)
  (GET "/oauth2-callback-google" [] oauth2-callback-google)
  (GET "/logout" [] logout)

  ; ## Public Pages
  ; ontology terms
  (GET "/ontology/*" [:as request] (ontology-get request))
  (POST "/ontology/*" [:as request] (ontology-post request))
  ; TODO: PUT terms

  ; doc directory
  (GET "/doc/:doc.html" [doc :as req] (render-doc req doc))
  (GET "/index.html" req (render-doc req "index"))
  (GET "/" req (render-doc req "index"))

  ; ## Dev Pages
  (GET "/dev/status" [] render-status)

  ; static resources
  (route/resources "")

  ; not found
  (route/not-found
   #(base-template
     (:session %)
     "Not Found"
     [:div
      [:h1 "Not Found"]
      [:p "Sorry, the page you requested was not found."]
      [:p [:a {:href "/"} "Return to home page."]]])))

; Start/Restart http://www.http-kit.org/server.html#stop-server
(defonce server (atom nil))

(defn serve
  "Load data and serve pages on a given port."
  []
  (println "Listening on" (:port @state) "...")
  (reset!
   server
   (httpkit/run-server
    (->> knode-routes
         wrap-session)
    {:port (Integer. (:port @state))})))

(defn stop
  []
  (when-not (nil? @server)
    (@server :timeout 100)
    (reset! server nil)))

(defn restart
  []
  (stop)
  (serve))
