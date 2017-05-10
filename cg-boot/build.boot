(set-env!
  :source-paths #{"src"}
  :dependencies '[[org.clojure/clojure "1.8.0"]
                  [quil "2.5.0"]
                  [nightlight "1.6.3" :scope "test"]])

(comment
(require '[nightlight.boot :refer [nightlight]])

(deftask nl []
  (comp (wait) (nightlight :port 4000))))
