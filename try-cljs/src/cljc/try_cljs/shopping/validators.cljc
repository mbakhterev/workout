(ns try-cljs.shopping.validators
  (:require [valip.core :refer [validate]]
            [valip.predicates :refer [present?
                                      integer-string?
                                      decimal-string?
                                      gt]]))

(defn- empty-checks []
  (map (fn [x] [(keyword x) present? (str x "can't be empty")])
       ["quantity" "price" "tax" "discount"]))

(defn- number-checks []
  (map (fn [x] [(keyword x) decimal-string? (str x " should be a number")])
       ["price" "tax" "discount"]))

(defn- quantity-checks []
 (list [:quantity integer-string? "quantity should be an integer"]
       [:quantity (gt 0) "quantity should be positive"]))

(defn validate-shopping-form [quantity price tax discount]
  (apply validate {:quantity quantity :price price :tax tax :discount discount}
                  (concat (empty-checks)
                          (number-checks)
                          (quantity-checks))))
