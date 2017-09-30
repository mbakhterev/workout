(ns teads (:gen-class))

(defn- dump [& args] (binding [*out* *err*] (apply println args)))

(defrecord Graph [edges ^ints degrees])

(defrecord Leaves [leaves internals])

(defrecord Edge [^int u ^int v])

(defn- load-graph-2 ^Graph []
  (let [s (java.util.Scanner. *in*)
        N (.nextInt s)
        [E M] (loop [n N  max-id 0 edges (transient [])]
                (if (zero? n)
                  [(persistent! edges) max-id]
                  (let [u (.nextInt s)
                        v (.nextInt s)]
                    (recur (- n 1) (max max-id u v) (conj! edges [u v])))))
        D (int-array (+ 1 M))]
    (doseq [[u v] E]
      (aset D u (+ 1 (aget D u)))
      (aset D v (+ 1 (aget D v))))
    (->Graph E D)))

(defn- filter-2 ^Graph [^Graph {D :degrees :as G}]
  (let [[E V] (loop [E-graph (:edges G) E-internal (transient []) verticies (transient [])]
                (if (empty? E-graph)
                  [(persistent! E-internal) (persistent! verticies)]
                  (let [[u v :as e] (first E-graph)
                        du (aget D u)
                        dv (aget D v)]
                    (if (and (< 1 du) (< 1 dv))
                      (recur (next E-graph) (conj! E-internal e) verticies)
                      (recur (next E-graph) E-internal (conj! verticies (if (= 1 du) v u)))))))]
    (doseq [v V]
      (aset D v (- (aget D v) 1)))
    (assoc G :edges E)))

(defn -main [& args]
  (let [init-G (load-graph-2)]
    (loop [G init-G steps 0]
      (if (not (empty? (:edges G)))
        (recur (filter-2 G) (+ 1 steps))
        (println steps)))))
