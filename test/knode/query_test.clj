(ns knode.query-test
  (:require [clojure.test :refer [deftest testing is]]
            [knode.server.query :as q]))

(deftest test--substitute-single-quotes
  (testing "Don't mess with double-quoted strings regardless of label contents"
    (is (= "Hello there \"keyword\"!"
           (q/-substitute-single-quotes
            (atom {:env {:labels {"keyword" {:curie "foo:bar" :iri "www.foo.com/bar"}}}})
            "Hello there \"keyword\"!")))
    (is (= "Hello there \"keyword\"!"
           (q/-substitute-single-quotes (atom {}) "Hello there \"keyword\"!"))))
  (testing "When no label found"
    (testing "Leaves in single quoted term"
      (is (= "Hello there 'keyword'!"
             (q/-substitute-single-quotes
              (atom {}) "Hello there 'keyword'!")))))
  (testing "When appropriate label found"
    (testing "When curie is present"
      (testing "Substitute curie for single-quoted string"
        (is (= "Hello there foo:bar!"
               (q/-substitute-single-quotes
                (atom {:env {:labels {"keyword" {:curie "foo:bar" :iri "www.foo.com/bar"}}}})
                "Hello there 'keyword'!")))))
    (testing "When curie is absent"
      (testing "Substitute pointied iri for single-quoted string"
        (is (= "Hello there <www.foo.com/bar>!"
               (q/-substitute-single-quotes
                (atom {:env {:labels {"keyword" {:iri "www.foo.com/bar"}}}})
                "Hello there 'keyword'!")))))))

(deftest sanitized-sparql
  (testing "Adds limits to queries without limit"
    (is (= (str "SELECT DISTINCT ?s WHERE { ?s ?property ?object . }
LIMIT " q/+default-limit+)
           (q/sanitized-sparql "SELECT DISTINCT ?s WHERE { ?s ?property ?object . }"))))
  (testing "Reduces limit to the default limit on queries with larger limits"
    (is (= (str "SELECT DISTINCT ?s WHERE { ?s ?property ?object . } LIMIT " q/+default-limit+)
           (q/sanitized-sparql "SELECT DISTINCT ?s WHERE { ?s ?property ?object . } LIMIT 278367826876332947"))))
  (testing "Adds limit and offset for queries with pages"
    (let [pg 2]
      (is (= (str "SELECT DISTINCT ?s WHERE { ?s ?property ?object . }
LIMIT " q/+default-limit+ "\nOFFSET " (* q/+default-limit+ pg))
             (q/sanitized-sparql "SELECT DISTINCT ?s WHERE { ?s ?property ?object . }" :page pg)))))
  (testing "Leaves given limit if that limit is less than the default limit"
    (is (= "SELECT DISTINCT ?s WHERE { ?s ?property ?object . } LIMIT 1"
           (q/sanitized-sparql "SELECT DISTINCT ?s WHERE { ?s ?property ?object . } LIMIT 1"))))
  (testing "Leaves given offset if it exists (is this desired behavior?)"
    (is (= "SELECT DISTINCT ?s WHERE { ?s ?property ?object . } LIMIT 1 OFFSET 30"
           (q/sanitized-sparql "SELECT DISTINCT ?s WHERE { ?s ?property ?object . } LIMIT 1 OFFSET 30")))))
