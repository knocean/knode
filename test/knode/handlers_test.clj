(ns knode.handlers-test
  (:require [clojure.test :refer [deftest testing is]]
            [knode.server.handlers :refer [string->bidi-path insert-new-handler]]))

(deftest insert-new-handler-test
  (testing "leaves the first component of a path slash-free"
    (is (= (insert-new-handler ["/" {}] (string->bidi-path "/test") :blah)
           ["/" {"test" :blah}])))
  (testing "handles path parameters"
    (is (= (insert-new-handler ["/" {}] (string->bidi-path "/foo/:bar/baz") :blah)
           ["/" {["foo/" :bar] {"/baz" :blah}}])))
  (testing "doesn't clobber existing paths while extending"
    (is (= (insert-new-handler ["/" {"test" :foo}] (string->bidi-path "/test/one/two") :bar)
           ["/" {"test" {"" :foo, "/one" {"/two" :bar}}}]))
    (is (= (insert-new-handler ["/" {["test/" :foo] :bar}] (string->bidi-path "/test/:foo/one/two") :mumble)
           ["/" {["test/" :foo] {"" :bar, "/one" {"/two" :mumble}}}])))
  (testing "doesn't clobber existing paths while defining overlapping, but non-extending paths"
    (is (= (insert-new-handler
            ["/" {"foo" {"/bar" {"/baz" :deep-handler}}}]
            (string->bidi-path "/foo") :shallow-handler)
           ["/" {"foo" {"/bar" {"/baz" :deep-handler}, "" :shallow-handler}}])))
  (testing "errors on perfecty conflicting paths"
    (is (thrown? Exception (insert-new-handler ["/" {"test" :foo}] (string->bidi-path "/test") :bar)))))

(deftest string->bidi-path-test
  (testing "prepends '/' to every path element other than the first")
  (testing "does not prepend '/' to the first path element")
  (testing "treats colonized path components as variables, adds them to the preceding path element as a vector component"))

(deftest routing-and-linking-test
  (testing "link-to returns the path to a given handler")
  (testing "link-to handles parameterized paths")
  (testing "route-request sends a request to the right handler")
  (testing "routes-handler stores path params in :path-params, and adds them to :params in the request object it propagates")
  (testing "routes-handler shows the :internal-error handler in the event of an error in the requested handler")
  (testing "routes-handler shows a plaintext, basic 500 error response in the event of an error in the default :internal-error handler"))
