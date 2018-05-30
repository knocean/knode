(ns com.knocean.knode.server-test
  (:require [clojure.test :refer [deftest testing is]]
            [bidi.bidi :as bidi]
            [com.knocean.knode.server :refer [routes] :as server]))

(deftest test-route-that-must-exist
  (doseq [route
          ["/"
           "/index.html"
           "/doc"
           "/doc/index.html"
           "/doc/api.html"
           "/ontology"
           "/ontology/ONTIE"
           "/ontology/ONTIE_0000001"
           "/resources"
           "/resources/all"
           "/resources/all/subject"
           "/resources/all/subjects"
           "/resources/all/predicates"]]
    (is (not (nil? (bidi/match-route routes route)))
        (str route " should not be nil"))
    (is (not= {:handler com.knocean.knode.server/not-found}
              (bidi/match-route routes route))
        (str route " should not be not-found"))))
