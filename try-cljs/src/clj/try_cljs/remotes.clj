(ns try-cljs.remotes
  (:require [try-cljs.core :refer [handler]]
            [compojure.handler :refer [site]]
            [shoreleave.middleware.rpc :refer [defremote wrap-rpc]]
            [try-cljs.login.validators :as v]))

(defremote calculate [quantity price tax discount]
  (comment (println "calculate: quantity price tax discount"))
  (-> (* quantity price)
      (* (+ 1 (/ tax 100)))
      (- discount)))

(defremote email-domain-errors [email] (v/email-domain-errors email))

(def app (site (wrap-rpc (var handler))))
