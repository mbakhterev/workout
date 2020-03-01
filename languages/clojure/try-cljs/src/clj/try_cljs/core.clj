(ns try-cljs.core
  (:require [compojure.core :refer [defroutes GET POST]]
            [compojure.route :refer [not-found files resources]]
            [compojure.handler :refer [site]]
            [try-cljs.login :refer [authenticate-user]]
            [try-cljs.templates.shopping :refer [shopping]]
            [shoreleave.middleware.rpc :refer [wrap-rpc]]))

(defroutes handler
  (GET "/" [] "Hello world")
  (files "/" {:root "/tmp/target"})
  (POST "/login" [email password] (authenticate-user email password))
  (POST "/shopping" [quantity price tax discount]
        (shopping quantity price tax discount))
  (resources "/" {:root "/tmp/target"})
  (not-found "Page not found"))

;   (POST "/shopping" [quantity price tax discount]
;         (str "data vector: " [quantity price tax discount]))


(def app (site (wrap-rpc (var handler))))
