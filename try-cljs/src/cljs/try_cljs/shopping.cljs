(ns try-cljs.shopping
  (:require [domina.core :refer [by-id by-class value
                                 set-value! append! destroy!]]
            [domina.events :refer [listen! prevent-default]]
            [hiccups.runtime]
            [shoreleave.remotes.http-rpc :refer [remote-callback]]
            [cljs.reader :refer [read-string]])
  (:require-macros [hiccups.core :refer [html]]))

(defn extract [] (map (comp value by-id) ["quantity" "price" "tax" "discount"]))

(defn calculate-inplace []
  (. js/console log "Calculating")
  (let [[quantity
         price
         tax
         discount] (map (comp value by-id)
                        ["quantity" "price" "tax" "discount"])]
    (set-value! (by-id "total") (-> (* quantity price)
                                    (* (+ 1 (/ tax 100)))
                                    (- discount)
                                    (.toFixed 2)))))

(defn calculate [e]
  (let [args (map (comp value by-id)
                  ["quantity" "price" "tax" "discount"])]
    (remote-callback :calculate
                     (vec args)
                     (fn [r] (set-value! (by-id "total") (.toFixed r 2)))))
  (prevent-default e))

; (defn ^:export init []
;   (. js/console log "Initiating")
;   (if (and js/document (. js/document -getElementById))
;     (set! (.-onsubmit (. js/document getElementById "shoppingForm")) calculate)))

(defn ^:export init []
  (. js/console log "Initiating")
  (when (and js/document
             (aget js/document "getElementById"))
    (let [calc-listen! (partial listen! (by-id "calc"))]
      (calc-listen! :click
                    (fn [e] (calculate e)))
      (calc-listen! :mouseover
                    (fn [e] (append! (by-id "shoppingForm")
                                     (html [:div.help "click to compute"]))))
      (calc-listen! :mouseout
                    (fn [e] (destroy! (by-class "help")))))))

; (set! (.-onload js/window) init)

; (fn [] (append! (by-id "shoppingForm")
;                 "<div class='help'>Click to recalculate.</div>")))

; (and js/document (. js/document -getElementById))
