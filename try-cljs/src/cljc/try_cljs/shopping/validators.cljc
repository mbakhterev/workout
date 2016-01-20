(ns try-cljs.shopping.validators
  (:require [valip.core :refer [validate]]
            [valip.predicates :refer [present?
                                      integer-string?
                                      decimal-string?
                                      gt]]))

(defmacro empty-checks []
  `(list ~@(map (fn [x] [(keyword x) 'present? (str x " can't be empty")])
                ["quantity" "price" "tax" "discount"])))

(defmacro number-checks []
  `(list ~@(mapv (fn [x] [(keyword x) 'decimal-string? (str x " should be a number")])
                 ["price" "tax" "discount"])))

(defmacro quantity-checks []
 `(list [:quantity integer-string? "quantity should be an integer"]
        [:quantity (gt 0) "quantity should be positive"]))

(defmacro checks [] `(list ~@(empty-checks)
                           ~@(number-checks)
                           ~@(quantity-checks)))

(defn validate-shopping-form [quantity price tax discount]
  (apply validate {:quantity quantity :price price :tax tax :discount discount}
                  (checks)))
