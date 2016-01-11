(ns try-cljs.core
  (:require [compojure.core :refer [defroutes GET]]
            [compojure.route :refer [not-found files resources]]))

(defroutes handler
  (GET "/" [] "Hello world")
  (files "/" {:root "/tmp/target"})
  (resources "/" {:root "/tmp/target"})
  (not-found "Page not found"))
