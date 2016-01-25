(ns try-cljs.shopping.validators
  (:require [valip.core :refer [validate]]
            [valip.predicates :refer [present?
                                      integer-string?
                                      decimal-string?
                                      gt]])
  #?(:cljs (:require-macros
             [try-cljs.shopping.validators :refer [validate-macro]])))

(defn- empty-checks []
  (map (fn [x] [(keyword x) 'present? (str x " can't be empty")])
       '("quantity" "price" "tax" "discount")))

(defn- number-checks []
  (map (fn [x] [(keyword x)
                'decimal-string?
                (str x " should be a number")])
       '("price" "tax" "discount")))

(defn- quantity-checks []
  (list [:quantity 'integer-string? "quantity should be an integer"]
        [:quantity '(gt 0) "quantity should be positive"]))

(defn- checks-fn [] (concat (empty-checks)
                            (number-checks)
                            (quantity-checks)))

(defmacro validate-macro [V C] `(validate ~V ~@(eval C)))

(defn validate-shopping-form [q p t d]
  (validate-macro {:quantity q :price p :tax t :discount d}
                  (checks-fn)))

(defn validate-shopping-field [field v]
  (first (field (case field
                  :quantity (validate-shopping-form v "1" "2" "3")
                  :price    (validate-shopping-form "1" v "2" "3")
                  :tax      (validate-shopping-form "1" "2" v "3")
                  :discount (validate-shopping-form "1" "2" "3" v)))))
