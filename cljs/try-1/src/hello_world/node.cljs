(ns hello-world.node
    (:require [cljs.nodejs :as nodejs]))

(nodejs/enable-util-print!)

(defn- -main [& args] (println "hello world"))

(set! *main-cli-fn* -main)
