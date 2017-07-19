(ns knode.query-test
  (:require [clojure.test :refer [deftest testing is]]
            [knode.server.query :as q]))

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
