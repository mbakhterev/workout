(ns try-cljs.shopping
  (:require [domina.core :refer [by-id value set-value!]]))

(defn calculate []
  (. js/console log "Calculating")
  (let [[quantity price tax discount] (map (comp value by-id)
                                           ["quantity" "price" "tax" "discount"])]
    (set-value! (by-id "total") (-> (* quantity price)
                                    (* (+ 1 (/ tax 100)))
                                    (- discount)
                                    (.toFixed 2)))
    false))

(defn ^:export init []
  (. js/console log "Initiating")
  (if (and js/document (. js/document -getElementById))
    (set! (.-onsubmit (. js/document getElementById "shoppingForm")) calculate)))

; (set! (.-onload js/window) init)
