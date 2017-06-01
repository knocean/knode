(ns knode.server-test
  (:require [clojure.test :refer [deftest testing is]]
            [ring.middleware.params :refer [params-request]]
            [ring.mock.request :refer [request body]]
            [clojure.data.json :as json]
            [knode.state]
            [knode.core :as core]
            [knode.sparql :as sparql]
            [knode.server.server :as server]
            [knode.server.util :as sutil]))

(defn ex [x] (str "https://example.com/ontology/EXAMPLE_" x))

(def test-state
  (assoc
   (knode.state/init
    {:root-dir "test/example/"
     :root-iri "https://example.com/"
     :idspace "EXAMPLE"})
   :env
   {:prefixes {"EXAMPLE" "https://example.com/ontology/EXAMPLE_"}}
   :terms
   {(ex "0000001")
    {:subject {:iri (ex "0000001")}
     :blocks [{:predicate {:iri "foo"} :object {:lexical "bar"}}]}}))

(defn test-parse
  [uri expected]
  (is (= (server/parse-ontology-request
          test-state
          (params-request (request :get uri)))
         expected)))

(deftest test-parse-request
  (test-parse
   "/ontology/EXAMPLE_0000001"
   {:method :get
    :style "IRI"
    :iri (ex "0000001")})
  (test-parse
   "/ontology/EXAMPLE_0000001.tsv"
   {:method :get
    :output-format "tsv"
    :style "IRI"
    :iri (ex "0000001")})
  (test-parse
   "/ontology/EXAMPLE_0000001?output-format=tsv"
   {:method :get
    :output-format "tsv"
    :style "IRI"
    :iri (ex "0000001")})
  (test-parse
   (str "/ontology/?IRI=eq." (ex "0000001") "&output-format=tsv")
   {:method :get
    :output-format "tsv"
    :style "IRI"
    :iri (ex "0000001")})
  (test-parse
   (str "/ontology/?IRI=in." (ex "0000001") "&output-format=tsv")
   {:method :get
    :output-format "tsv"
    :style "IRI"
    :iris [(ex "0000001")]})
  (test-parse
   "/ontology/?CURIE=EXAMPLE:0000001"
   {:error "CURIE query should start with 'eq.' or 'in.', not: CURIE=EXAMPLE:0000001"})
  (test-parse
   "/ontology/?CURIE=eq.FOO:BAR"
   {:method :get
    :style "CURIE"
    :iri "FOO:BAR"})
  (test-parse
   "/ontology/?CURIE=eq.EXAMPLE:0000001"
   {:method :get
    :style "CURIE"
    :iri (ex "0000001")})
  (test-parse
   "/ontology/?CURIE=in.EXAMPLE:0000001&output-format=tsv"
   {:method :get
    :output-format "tsv"
    :style "CURIE"
    :iris [(ex "0000001")]})
 ; (test-parse
 ;  {:uri "/ontology/"
 ;   :query-string "output-format=tsv"
 ;   :body "CURIE\nEXAMPLE:0000001"}
 ;  {:output-format "tsv"
 ;   :style "CURIE"
 ;   :iris [(ex "0000001")]})
  (test-parse
   "/ontology/EXAMPLE"
   {:method :get
    :style "IRI"
    :iris [(ex "0000001")]})
  (test-parse
   "/ontology/"
   {:method :get
    :style "IRI"
    :iris [(ex "0000001")]}))

(defn test-request
  [message req expected]
  (is (= (server/ontology-request! (atom test-state) (params-request req))
         expected)
      message))

(def add-term-string
  "EXAMPLE:label: Foo")

(def update-term-string
  (str ": EXAMPLE:0000001\n" add-term-string))

(def added-term
  {:subject {:iri (ex "0000002")}
   :blocks
   [{:predicate {:iri (ex "label")} :object {:lexical "Foo"}}]})

(def updated-term
  (assoc-in added-term [:subject :iri] (ex "0000001")))

(defn clean-term
  [term]
  {:subject {:iri (get-in term [:subject :iri])}
   :blocks (map core/minimal-statement (:blocks term))})

(defn mock-render-result
  [state req {:keys [term terms] :as result}]
  (cond
    term (assoc result :term (clean-term term))
    terms (assoc result :terms (map clean-term terms))
    :else result))

(deftest test-ontology-request
  (with-redefs [sparql/select
                (constantly
                 [{"subject" {:iri (ex "0000001")} "value" {:lexical "foo"}}
                  {"subject" {:iri (ex "0000001")} "value" {:lexical "bar"}}])
                server/render-result mock-render-result
                sutil/developer? (constantly true)
                server/validate-term (constantly nil)
                server/update-state! (fn [state-atom new-state] new-state)]
    (test-request
     "retrieve one term"
     (request :get "/ontology/EXAMPLE_0000001")
     {:term (get-in test-state [:terms (ex "0000001")])})

    (test-request
     "retrieve project terms"
     (request :get "/ontology/EXAMPLE")
     {:terms (sutil/get-terms test-state)})

    (test-request
     "filter for one term"
     (request :get "/ontology/?CURIE=eq.EXAMPLE:0000001")
     {:term (get-in test-state [:terms (ex "0000001")])})

    (test-request
     "filter for multiple terms"
     (request :get "/ontology/?CURIE=in.EXAMPLE:0000001")
     {:terms (sutil/get-terms test-state)})

    (test-request
     "filter for multiple terms"
     (body
      (request :post "/ontology/?method=get")
      "CURIE\nEXAMPLE:0000001")
     {:terms (sutil/get-terms test-state)})

    (test-request
     "select project terms and columns"
     (request :get "/ontology/EXAMPLE?select=CURIE,EXAMPLE:foo")
     {:column-headers ["CURIE" "EXAMPLE:foo"]
      :table
      [[[{:curie "EXAMPLE:0000001" :iri (ex "0000001")}]
        [{:lexical "bar"} {:lexical "foo"}]]]})

    (test-request
     "select terms and columns"
     (body
      (request :post "/ontology/EXAMPLE?select=CURIE,EXAMPLE:foo&method=get")
      "CURIE\nEXAMPLE:0000001\nFOO:BAR")
     {:column-headers ["CURIE" "EXAMPLE:foo"]
      :table
      [[[{:curie "EXAMPLE:0000001" :iri (ex "0000001")}]
        [{:lexical "bar"} {:lexical "foo"}]]
       [[{:curie "FOO:BAR" :iri "FOO:BAR"}]
        []]]})

    (test-request
     "validate a Knotation term"
     (body
      (request :post "/ontology/EXAMPLE_0000001")
      {:method "PUT"
       :input-format "kn"
       :term-string update-term-string
       :validate "Validate Term"})
     {:message "Term is valid"
      :term updated-term})

    (test-request
     "update a Knotation term"
     (body
      (request :put "/ontology/EXAMPLE_0000001")
      {:method "PUT"
       :input-format "kn"
       :term-string update-term-string
       :add "Add Term"})
     {:message "Term updated"
      :term updated-term})

    (test-request
     "create new Knotation term"
     (body
      (request :post "/ontology/EXAMPLE")
      {:input-format "kn"
       :term-string add-term-string
       :add "Add Term"})
     {:status 201
      :headers {"Location" "https://localhost/ontology/EXAMPLE_0000002"}
      :message "Term added"
      :term added-term})

    (test-request
     "retrieve non-existent term"
     (request :get "/ontology/FOO_001")
     nil)

    (test-request
     "update non-existent term"
     (body
      (request :post "/ontology/FOO_001")
      {:method "PUT"
       :input-format "kn"
       :term-string update-term-string
       :add "Add Term"})
     nil)))
