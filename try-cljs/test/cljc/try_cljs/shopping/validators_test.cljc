(ns try-cljs.shopping.validators-test
  (:require [try-cljs.shopping.validators :refer [validate-shopping-form
                                                  validate-shopping-field]]
            #?(:clj  [clojure.test :refer [deftest is are testing]]
               :cljs [cljs.test :refer-macros [deftest is are testing]]))

  #?(:cljs (:require-macros
             [try-cljs.shopping.validators-test :refer [gen-empty-tests]])))

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

(comment (defmacro gen-empty-tests []
  (concat '(are [expected actual] (= expected actual))
          (mapcat identity
                  (for [f ["quantity" "price" "tax" "discount"]
                        v ["" nil]]
                    [(str f " can't be empty")
                     (list 'validate-shopping-field (keyword f) v)])))))

(deftest validate-shopping-quantity-test
  (testing "shopping quantity validation"
    (testing "/ happy path"
      (are [expected actual] (= expected actual)
           nil (validate-shopping-field :quantity "20"))
    
    (testing "/ presence"
      ))))
