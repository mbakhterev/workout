(ns try-cljs.core
  (:require [compojure.core :refer [defroutes GET POST]]
            [compojure.route :refer [not-found files resources]]
            [try-cljs.login :refer [authenticate-user]]))

(defroutes handler
  (GET "/" [] "Hello world")
  (files "/" {:root "/tmp/target"})
  (POST "/login" [email password] (authenticate-user email password))
  (resources "/" {:root "/tmp/target"})
  (not-found "Page not found"))
