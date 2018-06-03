(ns com.knocean.knode.pages.resources
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [cheshire.core :as json]

            [org.knotation.rdf :as rdf]
            [org.knotation.environment :as en]
            [org.knotation.link :as ln]
            [com.knocean.knode.util :as util]
            [com.knocean.knode.state :refer [state] :as st]
            [com.knocean.knode.pages.html :refer [html]]
            [com.knocean.knode.pages.ontology.base :as base]
            [com.knocean.knode.pages.ontology.tsv :as tsv]))

(defn local-link
  [env resource iri]
  (if-let [curie (ln/iri->curie env iri)]
    (str "/resources/" resource "/subject?curie=" curie)
    (str "/resources/" resource "/subject?iri=" (util/escape iri))))

(defn render-object
  [resource env format {:keys [oi ob ol] :as state}]
  (cond
    oi (case format
         :CURIE [:a {:href (local-link env resource oi)} (ln/iri->curie env oi)]
         :label [:a {:href (local-link env resource oi)} (ln/iri->name env oi)]
         [:a {:href (local-link env resource oi)} oi])
    ob ob
    ol ol
    :else ""))

(defn iri->seq
  [resource env headers iri]
  (let [states (st/select
                (str
                 (when (not= "all" resource) (format "rt='%s' AND " resource))
                 (format "si='%s' ORDER BY id" iri)))]
    (into
     [:tr]
     (for [[column pi format] headers]
       (case column
         "IRI" [:td [:a {:href (local-link env resource iri)} iri]]
         "CURIE" [:td [:a {:href (local-link env resource iri)} (ln/iri->curie env iri)]]
         "recognized" [:td (->> states first boolean str)]
         (->> states
              (filter #(= pi (:pi %)))
              (map (partial render-object resource env format))
              distinct
              (interpose "|")
              (into [:td])))))))

(defn render-pair
  [env resource {:keys [pi oi ob ol] :as state}]
  [:li
   [:a {:href (local-link env resource pi)} (ln/iri->name env pi)]
   ": "
   (cond
     oi [:a {:href (local-link env resource oi)
             :rel pi
             :resource oi}
         (ln/iri->name env oi)]
     ob ob
     ol [:span {:property (ln/iri->curie env pi)} ol])])

(defn resource-entry
  [{:keys [label title description homepage]}]
  [:div.resource
   [:h3 [:a {:href (str "/resources/" label)} title]]
   [:p description]
   (when homepage [:p [:a {:href homepage} homepage]])])

(defn get-resource-entry
  [resource]
  (if (= "all" resource)
    {:label "all" :title "All Resources" :description "All resources loaded into this system."}
    (->> resource
         ;; TODO - figure out how to paginate intelligently
         (conj ["SELECT * FROM resources WHERE label=?"])
         st/query
         first)))

(defn resource-page
  [{:keys [session params] :as req}]
  (let [resource (get-resource-entry (:resource params))]
    (html
     {:title (:title resource)
      :content
      [:div.resource
       [:h2 (:title resource)]
       (when-let [description (:description resource)] [:p description])
       [:ul
        [:li [:a {:href (str "/resources/" (:label resource) "/subjects")} "Subjects"]]
        [:li [:a {:href (str "/resources/" (:label resource) "/predicates")} "Predicates"]]]
       (when (not= "all" (:label resource))
         (let [iri
               ;; TODO - cut literal IRIs out of this
               ;;        possibly move them into k-cljc (or just use those if they're already provided)
               (->> (format "SELECT si FROM states WHERE rt='%s' AND pi = 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type' AND oi = 'http://www.w3.org/2002/07/owl#Ontology' LIMIT 1" (:label resource))
                    st/query
                    first
                    :si)]
           (when iri
             (let [states
                   (->> (format "SELECT * FROM states WHERE rt='%s' AND si='%s'" (:label resource) iri)
                        st/query)
                   env (st/build-env-from-states states)]
               (->> states
                    (map (partial render-pair env (:label resource)))
                    (into [:ul])
                    (conj [:div [:h3 "Details"]]))))))]})))

(defn resources-page
  [{:keys [session] :as req}]
  (html
   {:session session
    :title "Resources"
    :content
    [:div
     [:h2 "Resources"]
     (->> "SELECT * FROM resources ORDER BY id"
          st/query
          (concat [{:label "all" :title "All Resources" :description "Query all available resources"}])
          (map resource-entry)
          (into [:div]))]}))

(def obo (partial apply str "http://purl.obolibrary.org/obo/"))

(def predicates
  [(rdf/rdf "type")
   (rdf/rdfs "label")
   (obo "IAO_0000118")
   (rdf/rdfs "subClassOf")
   (obo "ncbitaxon#has_rank")])

(defn subject-page
  [{:keys [params] :as req}]
  (if-let [iri (or (:iri req)
                   (get-in req [:params "iri"])
                   (when-let [curie (get-in req [:params "curie"])]
                     (ln/curie->iri (st/latest-env) curie)))]
    (if (= "html" (get params "format" "html"))
      (let [resource (get-in req [:params :resource] "all")
            resource-map (get-resource-entry resource)
            states
            (->> iri
                 (format "si='%s'" iri)
                 (str (when (not= "all" resource) (format "rt='%s' AND " resource)))
                 st/select
                 (map #(select-keys % [:si :sb :pi :oi :ob :ol :di :ln]))
                 distinct)
            env (st/build-env-from-states states)]
        (html
         {:title (ln/iri->name env iri)
          :content
          [:div.subject
           [:h2 (ln/iri->name env iri)]
           [:p [:b "IRI:"] " " [:a {:href iri} iri]]
           [:p "Showing data from "
            [:a {:href (str "/resources/" resource)} (:label resource-map)]
            ". "
            (when (not= resource "all")
              [:a {:href (local-link env "all" iri)} "Query all resources for this subject."])]
           (->> states
                (map :pi)
                (remove nil?)
                distinct
                (remove (set predicates))
                (into predicates)
                (mapcat
                 (fn [predicate]
                   (->> states
                        (filter #(= predicate (:pi %)))
                        (map (partial render-pair env resource)))))
                distinct
                (into [:ul]))
           [:div
            "Other formats: "
            [:a {:href (str (local-link env resource iri) "&format=ttl")} "Turtle (ttl)"]
            ", "
            [:a {:href (str (local-link env resource iri) "&format=json")} "JSON-LD (json)"]
            ", "
            [:a {:href (str (local-link env resource iri) "&format=tsv")} "TSV (tsv)"]
            "."]]}))
      ((base/with-requested-terms base/ontology-result)
       (assoc req :requested-iris [iri])))
    (html
     {:status 404
      :title "Subject not found"
      :content
      [:div
       [:h2 "Subject not found"]
       [:p "The subject could not be found"]]})))

(def default-limit 100)
(def max-limit 5000)

(defn get-limit
  [limit-string]
  (try
    (->> (or limit-string default-limit)
         str
         (Integer/parseInt)
         (min max-limit))
    (catch Exception e default-limit)))

(defn get-offset
  [offset-string]
  (try
    (->> (or offset-string 0)
         str
         (Integer/parseInt))
    (catch Exception e 0)))

(def ignore-keys [:resource "IRI" "CURIE" "show-headers" "compact" "select" "limit" "offset" "order" "format" "method"])

(defn build-condition-object
  [value]
  (let [[obj value] (if (.startsWith value "iri.")
                      ["oi" (subs value 4)]
                      ["ol" value])]
    (str
     obj
     (cond
       (.startsWith value "eq.")
       (format "='%s'" (subs value 3))
       (.startsWith value "like.")
       (format " LIKE '%s'" (-> value (subs 5) (string/replace "*" "%")))
       :else
       (throw (Exception. (str "Unhandled query parameter: " value)))))))

(defn build-condition
  [env column value]
  (case column
    "iri"  (if (re-matches #"in\.\(.*\)" value)
             (->> (string/split (subs value 4 (dec (count value))) #",")
                  (string/split #",")
                  (map string/trim)
                  (map #(str "'" % "'"))
                  (string/join ", ")
                  (format "si IN (%s)"))
             (throw (Exception. (str "Unhandled query value for 'iri': " value))))
    "curie"  (if (re-matches #"in\.\(.*\)" value)
               (->> (string/split (subs value 4 (dec (count value))) #",")
                    (map string/trim)
                    (map (partial ln/curie->iri env))
                    (map #(str "'" % "'"))
                    (string/join ", ")
                    (format "si IN (%s)"))
               (throw (Exception. (str "Unhandled query value for 'curie': " value))))
    "any" (build-condition-object value)
    (let [pi (ln/label->iri env column)]
      (when-not pi
        (throw (Exception. (str "Unknown column: " column))))
      (str
       (when pi (format "pi='%s' AND " pi))
       (build-condition-object value)))))

;; TODO - replace this with LDF machinery (looks like it does pretty much the same thing)
(defn build-query
  [env resource conditions]
  (->> (apply dissoc conditions ignore-keys)
       (map (partial apply build-condition env))
       (concat [(when (not= resource "all") (format "rt='%s'" resource))])
       (remove nil?)
       (interpose "AND")
       (#(when (first %) (concat ["WHERE"] %)))
       (concat ["SELECT DISTINCT si FROM states"])
       vec
       (#(conj % "ORDER BY si"))
       (#(conj % (str "LIMIT " (get-limit (get conditions "limit")))))
       (#(conj % (let [offset (get conditions "offset")]
                   (when-not (string/blank? offset)
                     (str "OFFSET " offset)))))
       (remove nil?)
       (string/join " ")))

(def default-select "IRI,label,obsolete,replacement")

(defn get-select
  [select-string compact]
  (cond
    select-string select-string
    (= compact "true") (string/replace default-select "IRI" "CURIE")
    :else default-select))

;; TODO - most of this should be done in the front-end rather than back-end JS craziness like the below
(defn build-form
  [{:keys [params] :as req}]
  (let [resource (:resource params)
        conditions (apply dissoc params ignore-keys)
        condition (if (first conditions)
                    (let [[predicate v] (first conditions)
                          [operator object]
                          (cond
                            (.startsWith v "iri.eq.") ["iri.eq." (subs v 7)]
                            (.startsWith v "iri.like.") ["iri.like." (subs v 9)]
                            (.startsWith v "iri.") ["iri." (subs v 4)]
                            (.startsWith v "eq.") ["eq." (subs v 3)]
                            (.startsWith v "like.") ["like." (subs v 5)]
                            :else ["eq." ""])]
                      {:predicate predicate
                       :operator operator
                       :object object})
                    {:predicate "" :operator "eq." :object ""})
        this-select (get-select (get params "select") (get params "compact"))]
    [:div.subject-search
     [:form.form-inline
      {:id "search" :action "subjects"}
      [:div.form-group
       [:label {:for "select"} "Select columns"]
       [:input.form-control {:id "select" :type "text" :size "40" :name "select" :value this-select}]]
      [:div.form-group
       [:label {:for "limit"} "Limit rows"]
       [:input.form-control {:id "limit" :type "text" :size "5" :name "limit" :value (get-limit (get params "limit"))}]]
      [:div.form-group
       [:label {:for "offset"} "Offset rows"]
       [:input.form-control {:id "offset" :type "text" :size "5" :name "offset" :value (get-offset (get params "offset"))}]]
      [:div.form-group
       [:label {:for "compact"} "Use CURIEs"]
       [:input.form-control
        (merge
         {:id "compact" :type "checkbox" :name "compact" :value "true" :onclick "if ($('#compact')[0].checked) {$('#select').val($('#select').val().replace('IRI','CURIE'))} else {$('#select').val($('#select').val().replace('CURIE','IRI'))};"}
         (when (= (get params "compact") "true") {:checked "true"}))]]
      [:input {:id "query" :type "hidden" :value "new query"}]]
     [:form.form-inline
      {:onsubmit "$('#query').attr('name', $('#predicate').val());
  $('#query').val($('#operator').val() + $('#object').val());
  $('#search').submit();
  return false;"}
      [:div.form-group
       [:label {:for "predicate"} "Predicate"]
       [:input.form-control {:id "predicate" :type "text" :name "predicate" :value (:predicate condition)}]]
      [:div.form-group
       [:label {:for "operator"} "Operator"]
       (let [op (:operator condition)]
         [:select.form-control
          {:id "operator" :name "operator" :value (:operator condition)}
          [:option {:value "eq." :selected (= op "eq.")} "literal equals"]
          [:option {:value "like." :selected (= op "like.")} "literal like"]
          [:option {:value "iri.eq." :selected (= op "iri.eq.")} "IRI equals"]
          [:option {:value "iri.like." :selected (= op "iri.like.")} "IRI like"]])]
      [:div.form-group
       [:label {:for "object"} "Object"]
       [:input.form-control {:id "object" :type "text" :name "object" :value (:object condition)}]]
      [:button.btn.btn-primary {:type "submit"} "Search"]
      [:button.btn.btn-warning
       {:onclick (format "
$('#select').val('%s');
$('#limit').val('100');
$('#offset').val('0');
$('#compact')[0].checked = false;
$('#predicate').val('');
$('#operator').val('eq.');
$('#object').val('');
return false" this-select)}
       "Reset"]]
     [:p
      [:b "Search tips: "]
      "Any of the " [:a {:href (str "/resources/" resource "/predicates")} "predicates"]
      " can be used for search or as a column in 'select'. "
      "The special " [:code "any"] " predicate can be used to search for any predicate. "
      "For the 'like' operators use " [:code "*"] " as a wildcard character. "]]))

(defn build-nav
  [page {:keys [query-params] :as req}]
  (let [limit (get-limit (get query-params "limit"))
        offset (try
                 (Integer/parseInt (get query-params "offset" "0"))
                 (catch Exception e 0))]
    [:div.subject-nav
     [:a
      (->> (dissoc query-params "offset")
           ring.util.codec/form-encode
           (str page "?")
           (assoc {} :href))
      "Start"]
     [:a
      (->> (- offset limit)
           (max 0)
           (assoc query-params "offset")
           ring.util.codec/form-encode
           (str page "?")
           (assoc {} :href))
      "Previous"]
     [:a
      (->> (+ offset limit)
           (assoc query-params "offset")
           ring.util.codec/form-encode
           (str page "?")
           (assoc {} :href))
      "Next"]]))

(defn subjects-page
  [{:keys [params query-params] :as req}]
  (let [req (if (:body-string req)
              req
              (assoc req :body-string (when (:body req) (-> req :body slurp))))
        env (st/latest-env)
        file-format (get params "format" "html")
        show-headers? (not (= (get params "show-headers") "false"))
        resource (get params :resource "all")
        resource-map (get-resource-entry resource)
        compact (= (get params "compact") "true")
        default (if compact :CURIE :IRI)
        specified-iris
        (cond
          (find params "IRI")
          (let [value (get params "IRI")]
            (if-let [[_ value] (re-matches #"in\.\((.*)\)" value)]
              (->> (string/split value #",")
                   (map string/trim))
              (throw (Exception. (str "Unhandled query value for 'IRI': " value)))))
          (find params "CURIE")
          (let [value (get params "CURIE")]
            (if-let [[_ value] (re-matches #"in\.\((.*)\)" value)]
              (->> (string/split value #",")
                   (map string/trim)
                   (map (partial ln/curie->iri env)))
              (throw (Exception. (str "Unhandled query value for 'CURIE': " value)))))
          (and (= :post (:request-method req))
               (= "GET" (get-in req [:params "method"]))
               (:body-string req))
          (let [lines (->> req :body-string string/split-lines (map string/trim))]
            (println "GET LINES")
            (case (first lines)
              "IRI" (rest lines)
              "CURIE" (map (partial ln/curie->iri env) (rest lines))
              (throw (Exception. (str "Unhandled POST body column: " (first lines)))))))
        iris (or specified-iris
                 (->> (build-query env resource params)
                      st/query
                      (map :si)))
        curie-column (when (:body-string req)
                       (->> req :body-string string/split-lines first string/trim (= "CURIE")))
        headers (if-let [select (get params "select")]
                  (tsv/parse-tsv-select env compact select)
                  (remove
                   nil?
                   [(if (or compact curie-column) ["CURIE" nil :CURIE] ["IRI" nil :IRI])
                    ["label" (rdf/rdfs "label") :label]
                    (when specified-iris ["recognized" nil :boolean])
                    ["obsolete" (rdf/owl "deprecated") :boolean]
                    ["replacement" "http://purl.obolibrary.org/obo/IAO_0100001" default]]))]
    (case file-format
      "tsv"
      {:headers
       {"Content-Type" "text/tab-separated-values"
        "Range-Unit" "items"
        "Content-Range"
        (let [offset (get-offset (get params "offset"))]
          (format "%d-%d/*" offset (max offset (dec (+ offset (count iris))))))}
       :body
       (->> iris
            (map (partial tsv/iri->seq env headers))
            (#(if show-headers?
                (cons (map first headers) %)
                %))
            tsv/tsv-join)}
      "html"
      (html
       {:title "Subjects"
        :content
        [:div
         [:h2 "Subjects for " (:title resource-map)]
         [:p
          [:a {:href (str "/resources/" resource)} "Back to " (:label resource-map) "."]
          " "
          (when (not= resource "all")
            [:a {:href (str "/resources/all/subjects?" (:query-string req))} "Query all resources."])]
         (build-form req)
         [:p
          (format "Showing %d results starting at %d. "
                  (count iris)
                  (get-offset (get params "offset")))
          [:a
           (->> (assoc query-params "format" "tsv")
                ring.util.codec/form-encode
                (str "subjects?")
                (assoc {} :href))
           "Download as TSV."]]
         (build-nav "subjects" req)
         (->> iris
              (map (partial iri->seq resource env headers))
              (concat
               [(->> headers
                     (map first)
                     (map (fn [x]
                            (if-let [iri (ln/->iri env x)]
                              [:th [:a {:href (local-link env resource iri)} x]]
                              [:th x])))
                     (into [:tr]))])
              (into [:table#results.table]))
         (build-nav "subjects" req)]})
      (html
       {:status 404
        :title "Unknown format"
        :error (str "unknown format: " format)}))))

(defn predicates-page
  [{:keys [params query-params] :as req}]
  (let [env (st/latest-env)
        file-format (get params "format" "html")
        show-headers? (not (= (get params "show-headers") "false"))
        resource (get params :resource "all")
        resource-map (get-resource-entry resource)
        compact (= (get params "compact") "true")
        default (if compact :CURIE :IRI)
        query (str "SELECT DISTINCT pi FROM states"
                   (when (not= "all" resource) (format " WHERE rt='%s'" resource))
                   " ORDER BY pi")
        iris (map :pi (st/query query))
        headers
        [(if compact ["CURIE" nil :CURIE] ["IRI" nil :IRI])
         ["label" (rdf/rdfs "label") :label]
         ["obsolete" (rdf/owl "deprecated") :boolean]
         ["replacement" "http://purl.obolibrary.org/obo/IAO_0100001" default]]]
    (case file-format
      "tsv"
      {:headers
       {"Content-Type" "text/tab-separated-values"
        "Range-Unit" "items"
        "Content-Range"
        (let [offset (get-offset (get params "offset"))]
          (format "%d-%d/*" offset (max offset (dec (+ offset (count iris))))))}
       :body
       (->> iris
            (map (partial tsv/iri->seq env headers))
            (#(if show-headers?
                (cons (map first headers) %)
                %))
            tsv/tsv-join)}
      "html"
      (html
       {:title "Predicates"
        :content
        [:div
         [:h2 "Predicates for " (:title resource-map)]
         [:p
          [:a {:href (str "/resources/" resource)} "Back to " (:label resource-map) "."]
          " "
          (when (not= resource "all")
            [:a {:href (str "/resources/all/predicates?" (:query-string req))} "Query all resources."])]
         [:p
          (format "Showing %d results. " (count iris))
          [:a
           (->> (assoc query-params "format" "tsv")
                ring.util.codec/form-encode
                (str "predicates?")
                (assoc {} :href))
           "Download as TSV."]]
         (->> iris
              (map (partial iri->seq resource env headers))
              (concat
               [(->> headers
                     (map first)
                     (map (fn [x]
                            (if-let [iri (ln/->iri env x)]
                              [:th [:a {:href (local-link env resource iri)} x]]
                              [:th x])))
                     (into [:tr]))])
              (into [:table.table]))]})
      (html
       {:status 404
        :title "Unknown format"
        :error (str "unknown format: " format)}))))

(defn predicates-page
  [req]
  (inner-predicates-page req))

(def routes
  [["/resources" resources-page]
   ["/resources/" resources-page]
   [["/resources/" :resource] #(resource-page %)]
   [["/resources/" :resource "/subject"] #(subject-page %)]
   [["/resources/" :resource "/subjects"] #(subjects-page %)]
   [["/resources/" :resource "/predicates"] #(predicates-page %)]])
