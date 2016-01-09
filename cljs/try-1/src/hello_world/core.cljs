(ns hello-world.core (:require [clojure.browser.repl :as repl]))

(defonce conn (repl/connect "http://localhost:9000/repl"))

(enable-console-print!)

(def hello-message (atom "test"))

(while true
  (println "hello from world" @hello-message)
  (sleep 1000))
