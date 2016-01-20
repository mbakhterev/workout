(ns try-cljs.shopping.validators
  (:require [valip.core :refer [validate]]
            [valip.predicates :refer [present?
                                      integer-string?
                                      decimal-string?
                                      gt]]
            [clojure.walk :refer [macroexpand-all]]))

(defmacro empty-checks-macro []
  (println "expanding" &form)
  (cons 'list
        (map (fn [x] [(keyword x)
                      'present?
                      (str x " can't be empty")])
             '("quantity" "price" "tax" "discount"))))

(defn- empty-checks []
  (map (fn [x] [(keyword x) 'present? (str x "can't be empty")])
       '("quantity" "price" "tax" "discount")))

(defmacro number-checks-macro []
  (println "expanding" &form)
  (cons 'list
        (map (fn [x] [(keyword x)
                      'decimal-string?
                      (str x " should be a number")])
             '("price" "tax" "discount"))))

(defn- number-checks []
  (map (fn [x] [(keyword x)
                'decimal-string?
                (str x " should be a number")])
       '("price" "tax" "discount")))

(defmacro quantity-checks-macro []
  (println "expanding" &form)
  '(list [:quantity integer-string? "quantity should be an integer"]
         [:quantity (gt 0) "quantity should be positive"]))

(defn- quantity-checks []
  (list [:quantity 'integer-string? "quantity should be an integer"]
        [:quantity '(gt 0) "quantity should be positive"]))

(defmacro checks-macro []
  (println "expanding" &form)
  (list 'concat '(empty-checks-macro)
                '(number-checks-macro)
                '(quantity-checks-macro)))

(defmacro checks []
  (println "expanding" &form)
  (cons 'list (concat (empty-checks)
                      (number-checks)
                      (quantity-checks))))

(defn- checks-fn [] (concat (empty-checks)
                            (number-checks)
                            (quantity-checks)))

(defmacro check-list [] (checks-fn))

(defmacro validate-macro [V C]
  (println "expanding" &form)
  `(validate ~V ~@(macroexpand C)))

(defn validate-shopping-form [quantity price tax discount]
  (validate-macro {:quantity quantity :price price :tax tax :discount discount}
                  (check-list)))

(comment (macroexpand-all
  '(defn validate-shopping-form [quantity price tax discount]
    (validate-macro {:quantity quantity :price price :tax tax :discount discount}
                    (check-list)))))

