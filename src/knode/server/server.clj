(ns knode.server.server
  (:require
   [clojure.java.io :as io]
   [clojure.string :as string]
   [clojure.data.json :as json]
   [clojure.data.csv :as csv]

   [org.httpkit.server :as httpkit]
   [compojure.route :as route]
   [ring.util.response :refer [redirect]]
   [ring.middleware.session :refer [wrap-session]]
   [ring.middleware.params :refer [wrap-params]]

   [hiccup.core :as hiccup]
   [hiccup.page :as pg]
   [markdown.core :as md]
   [yaml.core :as yaml]

   [knode.state :refer [state]]
   [knode.core :as core]
   [knode.emit :as emit]
   [knode.sparql :as sparql]
   [knode.util :as util]

   [knode.server.util :as sutil]
   [knode.server.template :refer [base-template]]
   [knode.server.authentication :as auth]
   [knode.server.upstream :as up])
  (:use [compojure.core :only [defroutes ANY GET POST PUT]]))

;; ## Ontology Term Rendering
;; ### HTML
(defn render-html-predicate
  [state req label {:keys [iri datatype cardinality]}]
  [:li
   [:a {:href (sutil/re-root state req iri)} label]
   " ("
   cardinality
   ", "
   (or (:curie datatype)
       (when (= datatype :link) "link")
       (when (= datatype :plain) "plain string"))
   ")"])

(defn render-html-cheatsheet
  [state req]
  [:div.cheatsheet.col-md-6
   [:div.templates
    "Templates (required predicates):"
    [:ul
     (for [template (-> state :templates vals)]
       [:li (:label template)
        " ("
        (string/join ", " (:required-predicates template))
        ")"])]]
   [:div.predicates
    "Predicates (cardinality, datatype):"
    [:ul
     (->> state
          :env
          :labels
          (filter #(:cardinality (second %)))
          (sort-by first)
          (map (partial apply render-html-predicate state req)))]]])

(defn render-html-term
  [state
   {:keys [params uri iri] :as req}
   {:keys [status headers error message term] :as result}]
  (let [old-term (get-in state [:terms iri] term)
        new-term (when-not (= old-term term) term)
        subject (:subject term)
        iri (:iri subject)
        label (get-in state [:env :iri-labels (:iri subject)])]
    {:status (or status 200)
     :headers (assoc headers "Content-Type" "text/html")
     :body
     (base-template
      req
      {:title label
       :content
       [:div
        [:h2 (:curie subject) " " label]
        [:p [:a {:href iri} iri]]
        (doall
         (emit/emit-rdfa-term
          (:env state)
          (:context state)
          (:subject old-term)
          (:blocks old-term)))
        [:p
         "Other formats: "
         [:a
          {:href (str (string/replace uri #".html^" "") ".ttl")}
          "Turtle (ttl)"]
         ", "
         [:a
          {:href (str (string/replace uri #".html^" "") ".json")}
          "JSON-LD (json)"]
         ", "
         [:a
          {:href (str (string/replace uri #".html^" "") ".tsv")}
          "TSV (tsv)"]
         "."]
        (when (sutil/developer? req)
          [:div.devlopment.container-fluid
           [:h3 "Edit Term"]
           [:div.edit.col-md-6
            (when message [:p message])
            (when error [:p error])
            (when (and new-term (get params "term-string"))
              [:form
               {:method "POST"}
               [:input {:type "hidden" :name "method" :value "PUT"}]
               [:input {:type "hidden" :name "input-format" :value "kn"}]
               [:input {:type "hidden"
                        :name "term-string"
                        :value (get params "term-string")}]
               (doall
                (emit/emit-rdfa-term
                 (:env state)
                 (:context state)
                 (:subject new-term)
                 (:blocks new-term)))
               [:p
                [:input
                 {:class "btn btn-primary"
                  :type "submit"
                  :name "add"
                  :value "Update Term"}]]])
            (when (:iri req)
              [:form
               {:method "POST"}
               [:input {:type "hidden" :name "method" :value "PUT"}]
               [:input {:type "hidden" :name "input-format" :value "kn"}]
               [:p
                [:textarea
                 {:name "term-string" :rows 6 :style "width: 100%"}
                 (or (get params "term-string")
                     (doall
                      (emit/emit-kn-term
                       (:env state)
                       nil
                       (:subject old-term)
                       (->> old-term :blocks (remove :template)))))]]
               [:p
                [:input
                 {:class "btn btn-primary"
                  :type "submit"
                  :name "validate"
                  :value "Validate Term"}]]])]
           (render-html-cheatsheet state req)])]})}))

(defn render-html-terms
  [state
   {:keys [params uri] :as req}
   {:keys [status headers error message term terms] :as result}]
  {:status (or status 200)
   :headers (assoc headers "Content-Type" "text/html")
   :body
   (base-template
    req
    {:title (:idspace state)
     :content
     [:div
      (when (sutil/developer? req)
        [:div.development.container-fluid
         [:h3 "Add Term"]
         [:div.edit.col-md-6
          (when message [:p message])
          (when error [:p error])
          (when term
            [:form
             {:method "POST"}
             [:input {:type "hidden" :name "input-format" :value "kn"}]
             [:input {:type "hidden"
                      :name "term-string"
                      :value (get params "term-string")}]
             (doall
              (emit/emit-rdfa-term
               (:env state)
               (:context state)
               nil
               (:blocks term)))
             [:p
              [:input
               {:class "btn btn-primary"
                :type "submit"
                :name "add"
                :value "Add Term"}]]])
          [:form
           {:method "POST"}
           [:input {:type "hidden" :name "input-format" :value "kn"}]
           [:p
            [:textarea
             {:name "term-string" :rows 6 :style "width: 100%"}
             (or
              (get params "term-string")
              (let [template (-> state :templates vals first)]
                (->> template
                     :required-predicates
                     (map #(str % ": REQUIRED"))
                     (concat
                      [(str "template: " (:label template))])
                     (string/join "\n"))))]]
           [:p
            [:input
             {:class "btn btn-primary"
              :type "submit"
              :name "validate"
              :value "Validate Term"}]]]]
         (render-html-cheatsheet state req)])
      [:h3 "Term List"]
      [:p
       "Other formats: "
       [:a
        {:href (str (string/replace uri #".html^" "") ".ttl")}
        "Turtle (ttl)"]
       ", "
           ;[:a
           ; {:href (str (string/replace uri #".html^" "") ".json")}
           ; "JSON-LD (json)"
           ;", "
       [:a
        {:href (str (string/replace uri #".html^" "") ".tsv")}
        "TSV (tsv)"]
       "."]
      [:ul
       (for [term terms]
         (let [subject (:subject term)
               iri (:iri subject)
               values (core/collect-values (:blocks term))]
           [:li
            [:a
             {:href (sutil/re-root state req iri)}
             (core/get-curie (:env state) iri)
             " "
             (get-in values [emit/rdfs:label 0 :lexical])]]))]]})})

(defn render-html-value
  [state req {:keys [curie iri lexical] :as value}]
  (if iri
    [:a {:href (sutil/re-root state req iri)} (or curie iri)]
    (or curie lexical)))

(defn render-html-table
  [state req {:keys [status headers error message table] :as result}]
  {:status (or status 200)
   :headers headers
   :body
   (base-template
    req
    {:title "Results"
     :error error
     :message message
     :content
     [:div
      [:p
       "Other formats: "
       [:a
        {:href (str (string/replace (:uri req) #".html^" "")
                    "?format=tsv"
                    (when (:query-string req)
                      (str "&" (:query-string req))))}
        "TSV (tsv)"]
       "."]
      [:table {:class "table"}
       [:tr
        (for [header (:column-headers result)]
          [:th header])]
       (for [row table]
         [:tr
          (for [values row]
            [:td
             (->> values
                  (map #(render-html-value state req %))
                  (interpose "|"))])])]]})})

(defn render-html-result
  [state
   {:keys [method params] :as req}
   {:keys [status headers error term terms table] :as result}]
  (cond
    (and (= :post method) (find params "validate"))
    (render-html-terms state req (assoc result :terms (sutil/get-terms state)))

    error {:status 400
           :body (base-template req {:title "Error" :error error})}
    table (render-html-table state req result)
    terms (render-html-terms state req result)

    (and term (= :post method))
    (let [iri (get-in result [:term :subject :iri])
          location (get-in result [:headers "Location"])]
      {:status status
       :headers headers
       :body
       (base-template
        req
        {:title "Term added"
         :content [:p "Added new term: " [:a {:href location} iri]]})})
    term (render-html-term state req result)))

;; ### Turtle
(defn render-ttl-result
  [state req {:keys [status headers error message term terms table] :as result}]
  (cond
    error {:status (or status 400) :body error}
    table {:status (or status 400) :body "Cannot render table to Turtle format"}
    terms {:status (or status 200)
           :headers (assoc headers "Content-Type" "text/turtle")
           :body
           (str
            (when message (str "# " message "\n\n"))
            (emit/emit-ttl-terms
             (:env state)
             (:context state)
             terms))}
    term {:status (or status 200)
          :headers (assoc headers "Content-Type" "text/turtle")
          :body
          (str
           (when message (str "# " message "\n\n"))
           (emit/emit-ttl-term
            (:env state)
            (:context state)
            (:subject term)
            (:blocks term)))}))

;; ### JSON-LD
(defn render-jsonld-result
  [state req {:keys [status headers error term terms table] :as result}]
  (cond
    error {:status (or status 400) :body error}
    table {:status (or status 400)
           :body "Cannot render table to JSON-LD format"}
    ; TODO: render multiple terms in JSON-LD
    terms {:status (or status 400)
           :body "Cannot render multiple term to JSON-LD format"}
    term {:status (or status 200)
          :headers (assoc headers "Content-Type" "application/json")
          :body
          (json/write-str
           (emit/emit-jsonld-term
            (:env state)
            (:context state)
            (:subject term)
            (:blocks term))
           :escape-slash false)}))

;; ### TSV
(defn seq->tsv-string
  [rows]
  (with-out-str (csv/write-csv *out* rows :separator \tab)))

(defn render-tsv-result
  [state req {:keys [status headers error term terms table] :as result}]
  (cond
    error {:status (or status 400) :body error}
    table {:status (or status 200)
           :headers headers
           :body
           (seq->tsv-string
            (concat
             (when-not
              (->> (get-in req [:params "show-headers"] "true")
                   string/lower-case
                   (= "false"))
               [(:column-headers result)])
             (sparql/table-seqs->strings table)))}))

(defn render-text-result
  [state req result]
  (str result))

(defn render-result
  [state {:keys [output-format] :as req} result]
  (when result
    (case (or output-format "html")
      "html" (render-html-result state req result)
      "ttl"  (render-ttl-result state req result)
      "json" (render-jsonld-result state req result)
      "tsv"  (render-tsv-result state req result)
      "text" (render-text-result state req result)
      nil)))

;; ## Build Ontology Term
(defn get-next-iri
  [iri-format iris]
  (->> (range 1 10000000)
       (map (partial format iri-format))
       (remove (set iris))
       first))

(defn blocks->term
  [state blocks]
  (if (->> blocks first :subject :iri)
    (let [iri (->> blocks first :subject :iri)
          curie (core/get-curie (:env state) iri)]
      {:subject {:iri iri :curie curie}
       :blocks (rest blocks)})
    (let [iri (get-next-iri (:term-iri-format state) (keys (:terms state)))
          curie (core/get-curie (:env state) iri)]
      {:subject {:iri iri :curie curie}
       :blocks blocks})))

(defn string->term
  [state input-format content]
  (case input-format
    "kn"
    (->> content
         string/split-lines
         (core/process-lines (:env state))
         second
         (core/expand-templates (:env state) (:templates state))
         (blocks->term state))
      ;else
    (throw (Exception. (str "Unknown input format: " input-format)))))

;; ## Update Ontology Terms
(defn validate-term
  [state term]
  (let [iri (get-in term [:subject :iri])
        test-state (assoc-in state [:terms iri] term)
        _ (sparql/load-terms! test-state)
        failure (sparql/validation-failure test-state)]
    (sparql/load-terms! state)
    failure))

;; ## Ontology Term Methods
(defn parse-request-output-format
  [{:keys [params uri] :as req}]
  (or
   (when (find params "output-format")
     (string/lower-case (get params "output-format")))
   (when (find params "format")
     (string/lower-case (get params "format")))
   (case (get-in req [:headers "accept"])
     "text/html" "html"
     "text/turtle" "ttl"
     "text/tab-separated-values" "tsv"
     "application/json" "json"
     nil)
   (when-let [[_ extension]
              (re-matches
               #"^.*\.(html|ttl|json|tsv)$"
               (string/lower-case (or uri "")))]
     extension)))

(defn parse-request-iri
  [state {:keys [params uri] :as req}]
  (or
   (when-let [[_ id] (re-matches #"/ontology/(\w+_\d+).*$" (or uri ""))]
     ["IRI" (str (:root-iri state) "ontology/" id)])
   (when (and (find params "IRI")
              (re-find #"^eq\." (get params "IRI")))
     ["IRI" (string/replace (get params "IRI") #"^eq\." "")])
   (when (and (find params "CURIE")
              (re-find #"^eq\." (get params "CURIE")))
     (let [curie (string/replace (get params "CURIE") #"^eq\." "")]
       ["CURIE"
        (or (core/resolve-curie-string (:env state) curie)
            curie)]))))

(defn parse-request-iris
  [state {:keys [params uri body] :as req}]
  (let [body (cond
               (nil? body) nil
               (string? body) body
               :else (slurp body))]
    (or
     (when (and (find params "IRI")
                (re-matches #"^in\..*" (get params "IRI")))
       ["IRI"
        (-> (get params "IRI")
            (string/replace #"^in\." "")
            (string/split #"\s+"))])
     (when (and (find params "CURIE")
                (re-matches #"^in\..*" (get params "CURIE")))
       ["CURIE"
        (map
         #(core/resolve-curie-string (:env state) %)
         (-> (get params "CURIE")
             (string/replace #"^in\." "")
             (string/split #"\s+")))])
     (when body
       (let [[header & rows] (string/split-lines body)]
         (cond
           (= header "CURIE")
           [header (map #(or (core/resolve-curie-string (:env state) %) %) rows)]
           (= header "IRI")
           [header rows]
           :else
           nil)))
     (when (-> (or uri "")
               (string/replace #"\.(html|tsv|json|ttl)$" "")
               (= (str "/ontology/" (:idspace state))))
       [(if (= "true" (get params "compact")) "CURIE" "IRI")
        (->> state :terms keys sort)])
     ; TODO: include external resources
     (when (= "/ontology/" uri)
       [(if (= "true" (get params "compact")) "CURIE" "IRI")
        (->> state :terms keys sort)]))))

(defn parse-ontology-request
  "Given a state and a request map,
   return a map with :format and :iris keys."
  [state {:keys [params request-method] :as req}]
  (cond
    (and (find params "IRI")
         (not (re-find #"^(eq|in)\." (get params "IRI"))))
    {:error
     (str
      "IRI query should start with 'eq.' or 'in.', not: IRI="
      (get params "IRI"))}

    (and (find params "CURIE")
         (not (re-find #"^(eq|in)\." (get params "CURIE"))))
    {:error
     (str
      "CURIE query should start with 'eq.' or 'in.', not: CURIE="
      (get params "CURIE"))}

    :else
    (merge
     (when-let [method
                (if (find params "method")
                  (-> (get params "method" "") string/lower-case keyword)
                  request-method)]
       {:method method})
     (when-let [output-format (parse-request-output-format req)]
       {:output-format output-format})
     (if-let [[style iri] (parse-request-iri state req)]
       {:style style :iri iri}
       (if-let [[style iris] (parse-request-iris state req)]
         {:style style :iris iris}
         {:error (str "Could not find IRI or IRIs for " req)})))))

(defn ontology-table-request
  [state {:keys [params style iri iris] :as req}]
  (let [compact (= "true" (get params "compact"))
        default (str style ",label,recognized,obsolete,replacement")
        select (get params "select" default)
        predicate-labels (string/split select  #",")
        predicates
        (map #(if (contains? #{"IRI" "CURIE" "recognized"} %)
                [% % nil]
                (try
                  (let [[_ name _ fmt]
                        (re-matches #"^(.*?)(\s*\[(\w+)\])?$" %)
                        iri (->> {:name (string/trim name)}
                                 (core/resolve-name (:env state))
                                 :iri)]
                    [% iri fmt])
                  (catch Exception e [% nil nil])))
             predicate-labels)
        predicate-iris (map second predicates)
        iris (or iris [iri])]
    (cond
      (empty? predicate-labels)
      {:error "Some predicates must be selected"}

      (empty? iris)
      {:error "Some terms must be submitted"}

      (not (util/all? predicate-iris))
      {:error
       (str
        "Some predicate labels cannot be resolved: "
        (->> predicates
             (filter #(nil? (second %)))
             (map first)
             (string/join ", ")))}

      :else
      {:column-headers predicate-labels
       :table
       (sparql/query-predicates state compact iris predicates)})))

(defn make-term
  [state params]
  (try
    (let [term (string->term
                state
                (get params "input-format")
                (get params "term-string"))]
      [term (validate-term state term)])
    (catch Exception e [nil {:label (.getMessage e)}])))

(defn ontology-request-handler
  [state {:keys [params error method output-format uri iri iris] :as req}]
  (try
    (cond
      error {:error error}

      ; GET
      (= :get method)
      (cond
        (or (= "tsv" output-format)
            (find params "select"))
        (ontology-table-request state req)
        iri (when-let [term (get-in state [:terms iri])]
              {:term term})
        iris {:terms (for [iri iris] (get-in state [:terms iri]))}
        :else nil)

      ; authenticate
      (and (contains? #{:put :post} method)
           (not (sutil/developer? req)))
      {:status 403 :error "Restricted to developers"}

      ; PUT
      (= :put method)
      (when (and iri (get-in state [:terms iri]))
        (let [[term failure] (make-term state params)
              iri (get-in term [:subject :iri])]
          (cond
            failure
            {:error (str "Validation failed: " (:label failure))}

            (not term)
            {:error (str "Term could not be created")}

            (not= iri (get-in term [:subject :iri]))
            {:error (str "Term IRIs must match")}

            (find params "validate")
            {:message "Term is valid" :term term}

            :else
            {:new-state (assoc-in state [:terms iri] term)
             :message "Term updated"
             :term term})))

      ; POST
      (= :post method)
      (when (= uri (str "/ontology/" (:idspace state)))
        (let [[term failure] (make-term state params)
              iri (get-in term [:subject :iri])]
          (cond
            failure
            {:error (str "Validation failed: " (:label failure))}

            (not term)
            {:error (str "Term could not be created")}

            (find params "validate")
            {:message "Term is valid" :term term}

            :else
            {:new-state (assoc-in state [:terms iri] term)
             :status 201
             :headers {"Location"
                       (sutil/re-root state req (get-in term [:subject :iri]))}
             :message "Term added"
             :term term})))

      :else nil)
    (catch Exception e {:status 400 :error (str e)})))

(defn update-state!
  [state-atom new-state]
  (spit
   (str (:root-dir new-state) "ontology/index.tsv")
   (emit/emit-index (:env new-state) (sutil/get-terms new-state)))
  (spit
   (str (:root-dir new-state) "ontology/" (:project-name new-state) ".kn")
   (emit/emit-kn-terms (:env new-state) nil (sutil/get-terms new-state)))
  (reset! state-atom new-state))

(defn ontology-request!
  [state-atom req]
  (let [req (merge (parse-ontology-request @state-atom req) req)
        {:keys [new-state] :as result}
        (ontology-request-handler @state-atom req)]
    (render-result
     (if new-state
       (update-state! state-atom new-state)
       @state-atom)
     req
     (dissoc result :new-state))))

;; ## Render Documentation
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
          {:title (:title metadata)
           :content (md/md-to-html-string content)}))))})

;; ## Status
(defn render-status
  [req]
  {:status 200
   :headers {"Content-Type" "text/html"}
   :body
   (let [s @state
         term-count (count (:terms s))]
     (base-template
      req
      {:title
       (cond (= 0 term-count) "Empty terms"
             :else "All systems go")
       :content
       [:ul [:li [:b "Terms Count"] "-" term-count]]}))})

;; ## Routes
(defroutes knode-routes
  ; ## Authentication
  (GET "/login" [] auth/login)
  (GET "/login-google" [] auth/login-google)
  (GET "/oauth2-callback-google" [] auth/oauth2-callback-google)
  (GET "/logout" [] auth/logout)

  ; ## Public Pages
  ; ontology terms
  (ANY "/ontology/*" [:as req] (ontology-request! state req))

  ; doc directory
  (GET "/doc/:doc.html" [doc :as req] (render-doc req doc))
  (GET "/index.html" req (render-doc req "index"))
  (GET "/" req (render-doc req "index"))

  ; ## Dev Pages
  (GET "/dev/status" [] render-status)
  (GET "/dev/upstream" [] up/render-upstream-report)

  ; static resources
  (route/resources "")

  ; not found
  (route/not-found
   #(base-template
     (:session %)
     {:title "Not Found"
      :content
      [:div
       [:h1 "Not Found"]
       [:p "Sorry, the page you requested was not found."]
       [:p [:a {:href "/"} "Return to home page."]]]})))

;; ## Server

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
         wrap-session
         wrap-params)
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