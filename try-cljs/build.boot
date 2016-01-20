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

(def ^:const defaults {:test-dirs (set (map #(str "test/clj" %) ["" "s" "c"]))
                       :output-to "main.js"
                       :testbed :phantom
                       :namespaces '#{try-cljs.shopping.validators-test
                                      try-cljs.login.validators-test}})

(deftask add-source-path [t dirs PATH #{str} ":source-paths"]
  (merge-env! :source-paths dirs)
  identity)

(deftask tdd
  [t dirs PATH #{str} "test paths"
   k httpkit bool "use http-kit server instead of jetty"
   v verbose bool "verbose report of changed files"
   p port PORT int "the web server port to listen"
   o output NAME str "js output file name"
   O opt LEVEL kw "optimization level"
   e testbed ENGINE kw "javascript testbed engine"
   n namespaces NS #{sym} "namespaces to run tests in"]
  (let [D (or dirs (defaults :test-dirs))
        testbed (or testbed (defaults :testbed))
        output (or output (defaults :output-to))
        namespaces (or namespaces (defaults :namespaces))]
    (comp (serve :handler 'try-cljs.core/app
                 :dir "/tmp/target"
                 :resource-root "/tmp/target"
                 :reload true
                 :httpkit httpkit
                 :port port)
          (add-source-path :dirs D)
          (watch :verbose verbose)
          (reload)
          (cljs-repl)
          (test-cljs :out-file output
                     :update-fs? true
                     :js-env testbed
                     :namespaces namespaces
                     :optimizations opt)
          (test :namespaces namespaces)
          (target :dir #{"/tmp/target"}))))
