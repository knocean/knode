(ns knode.server
  (:require
   [clojure.java.io :as io]
   [clojure.string :as string]
   [clojure.data.json :as json]

   [org.httpkit.server :as server]
   [compojure.route :as route]

   [hiccup.core :as hiccup]
   [hiccup.page :as pg]
   [markdown.core :as md]
   [yaml.core :as yaml]

   [knode.state :refer [state]]
   [knode.core :as core]
   [knode.emit :as emit])
  (:use [compojure.core :only [defroutes GET POST]]))

(defn stylesheet
  [name]
  [:link {:href (str "/assets/" name) :rel "stylesheet"}])

(defn base-template
  [title content]
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
        [:a {:class "navbar-brand" :href "/"} "Home"]]
       [:div {:id "navbar" :class "navbar-collapse collapse"}
        "<!--"
        [:ul {:class "nav navbar-nav"}
         [:li [:a {:href "ontie.html"} "ONTIE"]]
         [:li [:a {:href "api.html"} "API"]]]
        "-->"]]]
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
      (cond (= 0 term-count) "Empty terms"
            :else "All systems go")
      [:ul
       [:li [:b "Terms Count"] "-" term-count]
       (map (fn [[k v]] [:li [:b k] "-" v]) (dissoc s :terms))]))})

(defn render-html
  "Given a request, try to find a matching term,
   and return a response map for HTML."
  [req]
  (let [id (get-in req [:route-params :id])
        iri (str (:root-iri @state) "ontology/" id)
        term (get-in @state [:terms iri])
        subject (:subject term)
        label (get-in @state [:env :iri-labels (:iri subject)])]
    (when term
      {:status 200
       :headers {"Content-Type" "text/html"}
       :body
       (base-template
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
           {:href (str (string/replace id #".html^" "") ".ttl")}
           "Turtle (ttl)"]
          "."]])})))

(defn render-ttl
  "Given a request, try to find a matching term,
   and return a response map for Turtle."
  [req]
  (let [iri (str (:root-iri @state) "ontology/" (get-in req [:route-params :id]))
        term (get-in @state [:terms iri])]
    (when term
      {:status 200
       :headers {"Content-Type" "text/turtle"}
       :body
       (emit/emit-ttl-term
        (:env @state)
        (:context @state)
        (:subject term)
        (:blocks term))})))

(defn render-doc
  [doc]
  {:status 200
   :headers {"Content-Type" "text/html"}
   :body
   (let [path (str (:root-dir @state) "doc/" doc ".md")
         file (io/file path)]
     (when (.exists file)
       (let [[_ _ header content] (re-find #"(?s)(---(.*?)---)(.*)" (slurp file))
             metadata (yaml/parse-string header)]
         (base-template
          (:title metadata)
          (md/md-to-html-string content)))))})

(defn json-error
  [status & messages]
  {:status status
   :headers {"Content-Type" "application/json"}
   :body
   (json/write-str
    {:error (string/join " " messages)}
    :escape-slash false)})

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
        curie (core/get-curie (:env @state) iri)]
    {:subject {:iri iri :curie curie}
     :blocks
     (->> (concat
           [{:predicate {:label "template"}
             :object {:iri (:iri template)}}]
           (for [required (:required-predicates template)]
             {:predicate {:label required}
              :content (get data required)})
           ; TODO: support more optional predicates
           (for [synonym (get data "alternative term")]
             {:predicate {:label "alternative term"}
              :content synonym}))
          (map (partial core/resolve-block (:env @state)))
          (map (partial core/resolve-content (:env @state)))
          (core/expand-templates (:env @state) (:templates @state)))}))

(defn add-valid-term!
  [template data]
  (let [term (make-term template data)
        iri (get-in term [:subject :iri])]
    (swap! state assoc-in [:terms iri] term)
    (spit
     (str (:root-dir @state) "ontology/index.tsv")
     (emit/emit-index (:env @state) (:terms @state)))
    (spit
     (str (:root-dir @state) "ontology/" (:project-name @state) ".kn")
     (emit/emit-kn-terms (:env @state) nil (:terms @state)))
    {:status 201
     :headers {"Content-Type" "application/json"}
     :body
     (json/write-str
      {:iri iri
       :curie (get-in term [:subject :curie])}
      :escape-slash false)}))

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

(defroutes knode-routes
  ; ontology terms
  (GET "/ontology/:id.html" [id] render-html)
  (GET "/ontology/:id.ttl" [id] render-ttl)
  (GET "/ontology/:id" [id] render-html)

  ; doc directory
  (GET "/doc/:doc.html" [doc] (render-doc doc))
  (GET "/index.html" [] (render-doc "index"))
  (GET "/" [] (render-doc "index"))

  ; Dev API
  (POST "/dev/api/add-term" [] add-term-json!)
  (GET "/dev/status" [] render-status)

  ; static resources
  (route/resources "")

  ; not found
  (route/not-found
   (base-template
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
  (reset! server (server/run-server knode-routes {:port (:port @state)})))

(defn stop
  []
  (when-not (nil? @server)
    (@server :timeout 100)
    (reset! server nil)))

(defn restart
  []
  (stop)
  (serve))
