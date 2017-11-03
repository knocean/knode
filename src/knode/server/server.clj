(ns knode.server.server
  (:require
   [clojure.java.io :as io]
   [clojure.string :as string]
   [clojure.data.json :as json]
   [clojure.data.csv :as csv]

   [org.httpkit.server :as httpkit]
   [ring.util.response :refer [redirect]]
   [ring.middleware.session :refer [wrap-session]]
   [ring.middleware.params :refer [wrap-params]]
   [ring.middleware.head :refer [wrap-head]]

   [hiccup.core :as hiccup]
   [hiccup.page :as pg]
   [markdown.core :as md]
   [yaml.core :as yaml]
   [clj-jgit.porcelain :as git]
   [me.raynes.conch :as sh]

   [knode.server.handlers :as handlers]
   [knode.state :refer [state]]
   [knode.core :as core]
   [knode.emit :as emit]
   [knode.sparql :as sparql]
   [knode.util :as util]
   [knode.conf :as conf]

   [knode.server.util :as sutil]
   [knode.server.template :refer [base-template]]
   [knode.server.authentication :as auth]
   [knode.server.upstream :as up]
   [knode.server.query :as query]
   [knode.server.search :as search]
   [knode.server.tree-view :as tree]))

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

(defn html-term
  [env context subject blocks]
  (let [values (core/collect-values blocks)
        sorted-blocks (core/sort-blocks env blocks)]
    [:div
     (when (= "true" (get-in values [emit/owl:deprecated 0 :lexical]))
       [:p
        [:strong "OBSOLETE"]
        " This term is obsolete and should not be used for new data."])
     (when-let [iri (get-in values [emit/iao:replacement 0 :iri])]
       [:p
        [:strong "REPLACED BY " [:a {:href iri} iri]]])
     (doall
      (emit/emit-rdfa-term env context subject sorted-blocks))]))

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
        (html-term
         (:env state)
         (:context state)
         (:subject old-term)
         (:blocks old-term))
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
               (html-term
                (:env state)
                (:context state)
                (:subject new-term)
                (:blocks new-term))
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
  [state
   {:keys [params uri] :as req}
   {:keys [status headers error message table] :as result}]
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
      (when (= uri "/query")
        [:form
         {:method "GET" :action "/query"}
         [:input {:type "hidden" :name "compact" :value "true"}]
         [:textarea
          {:name "sparql"
           :rows (-> state :env :prefixes count (+ 6))
           :style "width: 100%"}
          (or
           (get params "sparql")
           (string/join
            "\n"
            (concat
             (for [[prefix iri] (-> state :env :prefixes)]
               (format "PREFIX %s: <%s>" prefix iri))
             [""
              "SELECT *"
              "WHERE {"
              "  ?s ?p ?o"
              "}"
              "LIMIT 10"])))]
         [:p
          [:input
           {:class "btn btn-primary"
            :type "submit"
            :name "query"
            :value "Run Query"}]]])
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
(defn get-sparql-labels!
  [iris]
  (let [raw (sparql/select-table
             @state true
             (clojure.pprint/cl-format
              nil
              "PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX owl: <http://www.w3.org/2002/07/owl#>
PREFIX obo: <http://purl.obolibrary.org/obo/>

SELECT ?subject ?label
WHERE {
  VALUES ?subject { ~{<~a>~^ ~} }
  ?subject rdfs:label ?label
}" iris))]
    (into {} (map (fn [[subject label]]
                    (let [s (first subject)]
                      [(:iri s) (assoc s :label (:lexical (first label)))]))
                  (rest raw)))))

(defn ensure-sparql-labels!
  [table]
  (let [missing-label-iris (->> table (map vals) (apply concat) (apply concat) (filter #(and (:iri %) (not (:label %)))) (map :iri) set)
        sparql-label-map (get-sparql-labels! missing-label-iris)
        replace-with-result (fn [entry]
                              (if-let [replacement (get-in sparql-label-map [(:iri entry) :label])]
                                (if (:label entry) entry (assoc entry :label replacement))
                                entry))]
    sparql-label-map
    (vec (map #(into {} (map (fn [[k vs]] [k (map replace-with-result vs)]) %)) table))))

(defn result->edn
  [{:keys [column-headers error table] :as result} & {:keys [transform] :or {transform identity}}]
  (let [res (for [row table]
              (into {} (map (fn [k vs]
                              [k (map transform vs)])
                            column-headers row)))]
    {:error error
     :headers column-headers
     :result (ensure-sparql-labels! res)}))

(defn ensure-edn-curie
  [state req {:keys [iri curie] :as edn}]
  (cond
    curie edn
    iri (assoc edn :curie (core/get-curie (:env state) iri))
    :else edn))

(defn ensure-edn-href
  [state req {:keys [iri href] :as edn}]
  (cond
    href edn
    iri (assoc edn :href (sutil/re-root state req iri))
    :else edn))

(defn ensure-edn-label
  [state req {:keys [label iri] :as edn}]
  (cond
    label edn
    iri (assoc edn :label (get-in state [:env :iri-labels iri]))
    :else edn))

(defn render-jsonld-table
  [{:keys [status] :as result} & {:keys [transform] :or {transform identity}}]
  ; TODO: render multiple terms in JSON-LD
  {:status (or status 200)
   :body (json/write-str (result->edn result :transform transform))})

(defn render-jsonld-result
  [state req {:keys [status headers error term terms table] :as result}]
  (cond
    error {:status (or status 400) :body error}
    table (render-jsonld-table
           result
           :transform (fn [res]
                        (ensure-edn-curie
                         state req
                         (ensure-edn-label
                          state req
                          (ensure-edn-href state req res)))))
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
(defn render-tsv-result
  [state req {:keys [status headers error term terms table] :as result}]
  (cond
    error {:status (or status 400) :body error}
    table {:status (or status 200)
           :headers headers
           :body
           (sutil/seq->tsv-string
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

(defn http-time
  [date]
  (when date
    (let [df (java.text.SimpleDateFormat.
              "EEE, dd MMM yyyy HH:mm:ss z"
              java.util.Locale/US)]
      (.setTimeZone df (java.util.TimeZone/getTimeZone "GMT"))
      (.format df date))))

(defn render-result
  [{:keys [last-modified] :as state}
   {:keys [output-format] :as req} result]
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
  [state {:keys [params] :as req}]
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
     (when-let [method (sutil/parse-request-method req)]
       {:method method})
     (when-let [output-format (sutil/parse-request-output-format req)]
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
  [{:keys [env] :as state}
   {:keys [params error method output-format uri iri iris] :as req}]
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
              iri (get-in term [:subject :iri])
              curie (core/get-curie env iri)]
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
             :log (str "Update term " curie)
             :term term})))

      ; POST
      (= :post method)
      (when (= uri (str "/ontology/" (:idspace state)))
        (let [[term failure] (make-term state params)
              iri (get-in term [:subject :iri])
              curie (core/get-curie env iri)]
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
             :log (str "Add term " curie)
             :term term})))

      :else nil)
    (catch Exception e {:status 400 :error (str e)})))

(defn update-state!
  [state-atom
   {:keys [ontology-dir project-name env git-repo] :as new-state}
   message username email]
  (let [index-path (str ontology-dir "index.tsv")
        term-path (str ontology-dir project-name ".kn")]
    (spit
     index-path
     (emit/emit-index env (sutil/get-terms new-state)))
    (spit
     term-path
     (emit/emit-kn-terms env nil (sutil/get-terms new-state)))
    (when (and git-repo message username email)
      (git/git-add git-repo index-path)
      (git/git-add git-repo term-path)
      (git/git-commit git-repo message {:name username :email email})
      ;; NOTE - In theory (git/with-identity {:private <slurped private key> :public <slurped public key>} (-> git-repo (.push) (.call)))
      ;;        should work here, but it keeps throwing JSchException USERAUTH fail errors, and I've decided that two hours of poking
      ;;        through
      ;;         http://download.eclipse.org/jgit/docs/jgit-2.0.0.201206130900-r/apidocs/org/eclipse/jgit/api/Git.html
      ;;        and http://clj-jgit.github.io/clj-jgit/
      ;;        is quite enough. So we're doing it the stupid way for now. FIXME later.
      (sh/with-programs [ssh-add git]
        (ssh-add conf/git-repo-key)
        (git (str "--git-dir=" (-> git-repo (.getRepository) (.getDirectory) (.toString))) "push" "origin" "master")))
    (reset!
     state-atom
     (assoc new-state :last-modified (java.util.Date.)))))

(defn query-request!
  [state-atom {:keys [params] :as req}]
  (let [method (sutil/parse-request-method req)
        compact (= "true" (get params "compact"))
        output-format (sutil/parse-request-output-format req)
        sparql (query/sanitized-sparql
                (get params "sparql")
                :page (try
                        (int (Float/parseFloat (get params "page")))
                        (catch Exception e 0)))]
    (render-result
     @state-atom
     (assoc req :method method :output-format output-format)
     (cond
       (not= :get method)
       {:error "Only GET is currently supported"}
       (nil? sparql)
       {:table []}
       :else
       (try
         (let [results (sparql/select-table @state-atom compact sparql)]
           {:column-headers (first results)
            :table (rest results)})
         (catch org.openrdf.query.MalformedQueryException e
           (let [cause (get (Throwable->map e) :cause)
                 [m col ln] (re-find #"line (\d+), column (\d+)" cause)]
             {:error (str "Malformed query. " cause)
              :location {:column col :line ln}})))))))
(handlers/intern-handler-fn! "/api/query" :query-request! #(query-request! state %))

(defn ontology-request!
  [state-atom req]
  (let [req (merge (parse-ontology-request @state-atom req) req)
        result (ontology-request-handler @state-atom req)
        state (if (:new-state result)
                (update-state!
                 state-atom
                 (:new-state result)
                 (:log result)
                 (get-in req [:session :name])
                 (get-in req [:session :email]))
                @state-atom)
        result (dissoc result :new-state)
        result (if (:last-modified state)
                 (assoc-in
                  result
                  [:headers "Last-Modified"]
                  (http-time (:last-modified state)))
                 result)]
    (render-result state req result)))
(handlers/intern-handler-fn! "/ontology/*" :ontology-request! #(ontology-request! state %))

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
(handlers/intern-handler-fn!
 "/doc/:doc" :render-doc #(render-doc % (string/replace (get-in % [:params "doc"] "") #"\.htm?$" "")))
(handlers/intern-handler-fn!
 ["/" "/index.html"] :render-index #(render-doc % "index"))

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
(handlers/intern-handler-fn! "/dev/status" :render-status render-status)

;; ## Server

; Start/Restart http://www.http-kit.org/server.html#stop-server
(defonce server (atom nil))

(defn handle-ontology-docs!
  []
  (handlers/intern-static!
   "/doc-static" (handlers/files (str (:root-dir @state) "doc"))))

(defn serve
  "Load data and serve pages on a given port."
  []
  (println "Listening on" (:port @state) "...")
  (reset!
   server
   (httpkit/run-server
    (->> handlers/routes-handler
         wrap-session
         wrap-params
         wrap-head)
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
