(ns try-cljs.remotes
  (:require [try-cljs.core :refer [handler]]
            [compojure.handler :refer [site]]
            [shoreleave.middleware.rpc :refer [defremote wrap-rpc]]))

(defremote calculate [quantity price tax discount]
  (comment (println "calculate: quantity price tax discount"))
  (-> (* quantity price)
      (* (+ 1 (/ tax 100)))
      (- discount)))

(def app (site (wrap-rpc (var handler))))
