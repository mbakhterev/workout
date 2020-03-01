(require 'cljs.repl)
(require 'cljs.build.api)
(require 'cljs.repl.browser)

(cljs.build.api/build "src" {:main 'hello-world.core
                             :output-to "/tmp/out/main.js"
                             :output-dir "/tmp/out"
                             :asset-path "/tmp/out"
                             :verbose true})

(cljs.repl/repl (cljs.repl.browser/repl-env)
                :watch "src"
                :output-dir "/tmp/out"
                :asset-path "/tmp/out")

(println "Hello")
