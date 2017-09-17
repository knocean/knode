(ns knode.handlers-test
  (:require [clojure.test :refer [deftest testing is thrown?]]
            [knode.server.handlers :refer [string->bidi-path insert-new-handler]]))

(deftest insert-new-handler-test
  (testing "leaves the first component of a path slash-free"
    (is (= (insert-new-handler ["/" {}] (string->bidi-path "/test") :blah)
           ["/" {"test" :blah}])))
  (testing "handles path parameters"
    (is (= (insert-new-handler ["/" {}] (string->bidi-path "/foo/:bar/baz") :blah)
           ["/" {["foo/" :bar] {"/baz" :blah}}])))
  (testing "doesn't clobber existing paths"
    (is (= (insert-new-handler ["/" {"test" :foo}] (string->bidi-path "/test/one/two") :bar)
           ["/" {"test" {"" :foo, "/one" {"/two" :bar}}}]))
    (is (= (insert-new-handler ["/" {["test/" :foo] :bar}] (string->bidi-path "/test/:foo/one/two") :mumble)
           ["/" {["test/" :foo] {"" :bar, "/one" {"/two" :mumble}}}])))
  ;; (testing "errors on perfectly conflicting paths"
  ;;   (is (thrown? Exception (insert-new-handler ["/" {["test/" :foo] :bar}] (string->bidi-path "/test/:baz") :mumble))))
  )
