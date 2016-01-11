(set-env!
  :source-paths #{"src/cljs" "src/clj"}
  :resource-paths #{"html"}
  :dependencies '[[org.clojure/clojure "1.7.0"]
                  [org.clojure/clojurescript "1.7.170"]
                  [adzerk/boot-cljs "1.7.170-3"]
                  [pandeiro/boot-http "0.7.0"]
                  [adzerk/boot-reload "0.4.2"]
                  [adzerk/boot-cljs-repl "0.3.0"]
                  [com.cemerick/piggieback "0.2.1"]
                  [weasel "0.7.0"]
                  [org.clojure/tools.nrepl "0.2.12"]
                  [org.clojars.magomimmo/domina "2.0.0-SNAPSHOT"]
                  [hiccups "0.3.0"]
                  [compojure "1.4.0"]
                  [org.clojars.magomimmo/shoreleave-remote-ring "0.3.1"]
                  [org.clojars.magomimmo/shoreleave-remote "0.3.1"]])

(require '[adzerk.boot-cljs :refer [cljs]]
         '[pandeiro.boot-http :refer [serve]]
         '[adzerk.boot-reload :refer [reload]]
         '[adzerk.boot-cljs-repl :refer [cljs-repl start-repl]])

(deftask dev []
  (comp (serve :dir "/tmp/target"
               :handler 'try-cljs.core/handler
               :resource-root "/tmp/target"
               :reload true)
        (watch)
        (reload)
        (cljs-repl)
        (cljs)
        (target :dir #{"/tmp/target"})))
