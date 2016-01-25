(ns try-cljs.shopping
  (:require [domina.core :refer [by-id
                                 by-class
                                 value
                                 text
                                 set-value!
                                 set-text!
                                 append!
                                 destroy!
                                 add-class!
                                 remove-class!]]
            [domina.events :refer [listen! prevent-default]]
            [domina.css :refer [sel]]
            [hiccups.runtime]
            [shoreleave.remotes.http-rpc :refer [remote-callback]]
            [cljs.reader :refer [read-string]]
            [try-cljs.shopping.validators :refer [validate-shopping-field
                                                  validate-shopping-form]])
  (:require-macros [hiccups.core :refer [html]]
                   [shoreleave.remotes.macros :as macros]))

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

(defn calculate! [e]
  (let [args (map (comp value by-id) ["quantity" "price" "tax" "discount"])
        errors (apply validate-shopping-form args)]
    (if-not errors
      (remote-callback :calculate
                       (vec args)
                       (fn [r] (set-value! (by-id "total") (.toFixed r 2))))))
  (prevent-default e))

(defn validate-shopping-field! [field original]
  (let [attr (name field)
        label (sel (str "label[for=" attr "]"))]
    (remove-class! label "help")
    (if-let [error (validate-shopping-field field (value (by-id attr)))]
      (do (add-class! label "help")
          (set-text! label error))
      (set-text! label original))))

(defn ^:export init []
  (. js/console log "Initiating")
  (when (and js/document
             (aget js/document "getElementById"))

    (. js/console log "Initiating fields validation")
    (let [listen-blur! (fn [i f] (listen! (by-id i) :blur f))
          [q-text
           p-text
           t-text
           d-text] (map (comp text sel (fn [x] (str  "label[for=" x "]")))
                        ["quantity" "price" "tax" "discount"])]
      (listen-blur! "quantity"
                    (fn [e] (validate-shopping-field! :quantity q-text)))

      (listen-blur! "price"
                    (fn [e] (validate-shopping-field! :price p-text)))

      (listen-blur! "tax"
                    (fn [e] (validate-shopping-field! :tax t-text)))

      (listen-blur! "discount"
                    (fn [e] (validate-shopping-field! :discount d-text))))

    (. js/console log "Initiating calculate button")
    (let [listen-calc! (partial listen! (by-id "calc"))]
      (listen-calc! :click
                    (fn [e] (calculate! e)))

      (listen-calc! :mouseover
                    (fn [e] (append! (by-id "shoppingForm")
                                     (html [:div#help.help "click to compute"]))))

      (listen-calc! :mouseout
                    (fn [e] (destroy! (by-id "help")))))))
