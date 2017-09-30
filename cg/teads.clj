(ns Solution (:gen-class))

(defmacro ^:private dump-times [code] `(binding [*out* *err*] (time ~code)))

(defn- dump [& args] (binding [*out* *err*] (apply println args)))

(defrecord Graph [edges ^ints degrees ^ints notes])

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
      (aset-int D u (+ 1 (aget D u)))
      (aset-int D v (+ 1 (aget D v))))
    (->Graph E D (int-array (+ 1 M)))))

(defn- filter-2 ^Graph [^Graph {D :degrees N :notes :as G}]
  (let [[E V] (loop [E-graph (:edges G) E-internal (transient []) verticies (transient [])]
                (if (empty? E-graph)
                  [(persistent! E-internal) (persistent! verticies)]
                  (let [[u v :as e] (first E-graph)
                        du (aget D u)
                        dv (aget D v)]
                    (if (and (< 1 du) (< 1 dv))
                      (recur (next E-graph) (conj! E-internal e) verticies)
                      (let [w (if (= 1 du) v u)
                            nw (aset-int N w (+ 1 (aget N w)))]
                        (recur (next E-graph)
                               E-internal
                               (if (= 1 nw) (conj! verticies w) verticies)))))))]
    (doseq [v V]
      (aset-int D v (- (aget D v) (aget N v)))
      (aset-int N v 0))
    (assoc G :edges E)))

(defn -main [& args]
  (let [init-G (dump-times (load-graph-2))]
    (loop [G init-G steps 0]
      (dump steps)
      (if (not (empty? (:edges G)))
        (recur (dump-times (filter-2 G)) (+ 1 steps))
        (println steps)))))
