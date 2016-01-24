(ns try-cljs.shopping.validators
  (:require [valip.core :refer [validate]]
            [valip.predicates :refer [present?
                                      integer-string?
                                      decimal-string?
                                      gt]])
  #?(:cljs (:require-macros
             [try-cljs.shopping.validators :refer [validate-macro
                                                   check-list]])))

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

(defmacro check-list [] (cons 'list (checks-fn)))

(defmacro validate-macro [V C]
  (comment (do (println "expanding" &form)
               (println `(validate ~V ~@(eval C)))))
  `(validate ~V ~@(eval C)))

(defn validate-shopping-form [q p t d]
  (validate-macro {:quantity q :price p :tax t :discount d}
                  (checks-fn)))

(comment (defn validate-shopping-form [q p t d]
  (apply validate {:quantity q :price p :tax t :discount d} (check-list))))

(comment (println (macroexpand '(validate-macro {} (checks-fn)))))
