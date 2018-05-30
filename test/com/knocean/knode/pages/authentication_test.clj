(ns com.knocean.knode.pages.authentication-test
  (:require [clojure.test :refer [deftest testing is]]
            [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as gen]
            [clojure.string :as string]

            [com.knocean.knode.pages.authentication :as auth]))

(s/def ::full-string (s/and string? #(> (count %) 0)))

(s/def ::name ::full-string)
(s/def ::email ::full-string)
(s/def ::id ::full-string)
(s/def ::session (s/keys :req-un [::name ::email ::id]))
(s/def ::authed-req (s/keys :req-un [::session]))

(s/fdef
 auth/logged-in?
 :args ::authed-req
 :ret boolean?)

(defn a-req []
  (gen/generate (s/gen ::authed-req)))

;(deftest test-logged-in?
;  (testing "Returns true when given a map with a session that contains :name, :email and :id slots"
;    (is (auth/logged-in? (a-req))))
;  (testing "Returns false when given a map with no session"
;    (is (not (auth/logged-in? (dissoc (a-req) :session)))))
;  (testing "Returns false when given a session that doesn't contain all three necessary keys"
;    (doseq [k [:name :email :id]]
;      (is (not (auth/logged-in? (update-in (a-req) [:session] #(dissoc % k))))))))

;; TODO - adding these tests involves centralizing specs somewhere. (We'd otherwise have to re-define ::request here
;; (deftest test-logged-in-only?
;;   (let [f (fn [req] :dummy-response)]
;;     (testing "Wraps a function of req->response with a logged-in checker"
;;       (testing "Wrapped function returns its usual value when the given request is logged in"
;;         (is (let [req (a-req)]
;;               (= ((auth/logged-in-only f) req)
;;                  (f req)))))
;;       (testing "Wrapped function returns a 400 response otherwise"
;;         (let [req (dissoc (a-req) :session)]
;;           (is (= 400 (:status ((auth/logged-in-only f) req)))))))))
