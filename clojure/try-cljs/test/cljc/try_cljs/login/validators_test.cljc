(ns try-cljs.login.validators-test
  (:require [try-cljs.login.validators :as v :refer [user-credentials-errors]]
            #?(:clj [clojure.test :refer [deftest are testing]]
               :cljs [cljs.test :refer-macros [deftest are testing]])))

(deftest user-credentials-errors-test
  (testing "login form validation"
    (testing "/ happy path"
      (are [expected actual] (= expected actual)
           nil (user-credentials-errors "me@ya.ru" "passw0rd")))))

#?(:clj (deftest email-domain-errors-test
          (testing "email domain checking"
            (are [expected actual] (= expected actual)
                 "mail domain isn't resolvable"
                 (first (:email (v/email-domain-errors "me@mur-mur-k.ru")))))))

