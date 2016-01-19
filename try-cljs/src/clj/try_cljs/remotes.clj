(ns try-cljs.remotes
  (:require ; [try-cljs.core :refer [handler]]
            ; [compojure.handler :refer [site]]
            [shoreleave.middleware.rpc :refer [defremote]]
            [try-cljs.login.validators :as v]))

(defremote calculate [quantity price tax discount]
  (comment (println "calculate: quantity price tax discount"))
  (let [[q p t d] (map read-string [quantity price tax discount])]
    (-> (* q p)
        (* (+ 1 (/ t 100)))
        (- d))))

(defremote email-domain-errors [email] (v/email-domain-errors email))

; (def app (site (wrap-rpc (var handler))))

(+ 1 2)
