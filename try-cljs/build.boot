(set-env!
  :source-paths #{"src/cljs" "src/clj" "src/cljc"}
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
                  [org.clojars.magomimmo/shoreleave-remote "0.3.1"]
                  [javax.servlet/servlet-api "2.5"]
                  [org.clojars.magomimmo/valip "0.4.0-SNAPSHOT"]
                  [enlive "1.1.6"]
                  [adzerk/boot-test "1.0.7"]
                  [crisptrutski/boot-cljs-test "0.2.1"]])

(require '[adzerk.boot-cljs :refer [cljs]]
         '[pandeiro.boot-http :refer [serve]]
         '[adzerk.boot-reload :refer [reload]]
         '[adzerk.boot-cljs-repl :refer [cljs-repl start-repl]]
         '[adzerk.boot-test :refer [test]]
         '[crisptrutski.boot-cljs-test :refer [test-cljs]])

(deftask dev []
  (comp (serve :dir "/tmp/target"
               :handler 'try-cljs.core/app
               :resource-root "/tmp/target"
               :reload true)
        (watch)
        (reload)
        (cljs-repl)
        (cljs)
        (target :dir #{"/tmp/target"})))

(deftask testing []
  (set-env! :source-paths (fn [p] (conj p "test/cljc")))
  identity)

(deftask tdd []
  (comp (serve :handler 'try-cljs.core/app
               :dir "/tmp/target"
               :resource-root "/tmp/target"
               :reload true)
        (testing)
        (watch)
        (reload)
        (cljs-repl)
        (test-cljs :out-file "main.js"
                   :update-fs? true
                   :js-env :phantom
                   :namespaces '#{try-cljs.shopping.validators-test})
        (test :namespaces '#{try-cljs.shopping.validators-test})
        (target :dir #{"/tmp/target"})))
