(require 'cljs.build.api)

(cljs.build.api/build "src" {:main 'hello-world.node
                             :output-to "main.js"
                             :target :nodejs})
