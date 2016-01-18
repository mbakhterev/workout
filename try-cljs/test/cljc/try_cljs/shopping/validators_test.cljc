(ns try-cljs.shopping.validators-test
  (:require [try-cljs.shopping.validators :refer [validate-shopping-form]]
            #?(:clj  [clojure.test :refer [deftest is are testing]]
               :cljs [cljs.test :refer-macros [deftest is are testing]])))

(deftest validate-shopping-form-test
  (testing "shopping form validation"

    (testing "/ happy path"
      (are [expected actual] (= expected actual)
           nil (validate-shopping-form "1" "0" "0" "0")
           nil (validate-shopping-form "1" "0.0" "0.0" "0.0")
           nil (validate-shopping-form "100" "100.25" "8.25" "123.45")))
    
    (testing "/ value type"
      (are [expected actual] (= expected actual)

           "quantity should be an integer"
           (first (:quantity (validate-shopping-form "foo" "0" "0" "0.0")))
           
           "price should be a number"
           (first (:price (validate-shopping-form "1" "foo" "0" "0")))))))
