(ns Solution (:gen-class))

(defrecord Graph [edges ^ints out-degrees])

(comment (defmacro ^:private dump-time [code] `(binding [*out* *err*] (time ~code))))

(def ^:const ^:private dump-time identity)

(defn- dump [& args] (binding [*out* *err*] (apply println args)))

(defn- load-graph ^Graph []
  (let [s (java.util.Scanner. *in*)
        N (.nextInt s)
        [E M] (loop [n N id-max 0 edges (transient [])]
                (if (zero? n)
                  [(persistent! edges) id-max]
                  (let [u (.nextInt s)
                        v (.nextInt s)]
                    (recur (- n 1) (max id-max u v) (conj! edges [u v])))))
        D (int-array (+ 1 M))]
    (doseq [[u v] E]
      (aset-int D u (+ 1 (aget D u))))
    (->Graph E D)))

(defn- filter-sinks ^Graph [^Graph {D :out-degrees :as G}]
  (let [[E V] (loop [g-edges (:edges G) edges (transient []) verticies (transient [])]
           (if (empty? g-edges)
             [(persistent! edges) (persistent! verticies)]
             (let [[u v :as e] (first g-edges)]
               (if (zero? (aget D v))
                 (recur (next g-edges) edges (conj! verticies u))
                 (recur (next g-edges) (conj! edges e) verticies)))))]
    (doseq [v V]
      (aset-int D v (- (aget D v) 1)))
    (assoc G :edges E)))

(defn -main [& args]
  (let [G-init (dump-time (load-graph))]
    (loop [G G-init steps 0]
;       (dump (:edges G))
;      (dump (vec (:out-degrees G))) 
      (if (not (empty? (:edges G)))
        (recur (dump-time (filter-sinks G)) (+ 1 steps))
        (println (+ 1 steps))))))
