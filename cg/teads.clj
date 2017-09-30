(ns teads (:gen-class))

(defn- dump [& args] (binding [*out* *err*] (apply println args)))

(defrecord Graph [edges ^ints degrees])

(defrecord Leaves [leaves internals])

(defrecord Edge [^int u ^int v])

(letfn [(normalize [[^long u ^long v :as e]] (if (<= u v) (vec e) [v u]))]
  (defn- load-graph-1 ^Graph []
    (let [E (partition 2 (repeatedly (* 2 (read)) read)) ; (seq (set (map normalize edges)))
          M (reduce (fn ^long [^long M [^long u ^long v]] (max M u v)) 0 E)
          D (int-array (+ 1 M))]
      (doseq [[u v] E]
        (aset D u (+ 1 (aget D u)))
        (aset D v (+ 1 (aget D v))))
      (->Graph E D))))

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
;    (dump "N:" N "edges:" (count E))
    (doseq [[u v] E]
      (aset D u (+ 1 (aget D u)))
      (aset D v (+ 1 (aget D v))))
    (->Graph E D)))

(defn- load-graph-3 ^Graph []
  (let [s (java.util.Scanner. *in*)
        [E M D-map] (loop [N (.nextInt s) max-id 0 edges (transient []) degrees (transient {})]
                      (if (zero? N)
                        [(persistent! edges) max-id (persistent! degrees)]
                        (let [u (.nextInt s)
                              v (.nextInt s)]
                          (recur (- N 1)
                                 (max max-id u v)
                                 (conj! edges [u v])
                                 (assoc! degrees
                                         u (+ 1 (get degrees u 0))
                                         v (+ 1 (get degrees v 0)))))))
        D (int-array (+ 1 M))]
    (doseq [[u N] D-map] (aset D u N))
    (->Graph E D)))

(defn- filter-leaves ^Graph [^Graph {E :edges D :degrees :as G}]
  (let [one (fn ^Leaves [^Leaves L [^long u ^long v :as e]]
              (if (and (< 1 (aget D u))
                       (< 1 (aget D v)))
                (assoc L :internals (conj (:internals L) e))
                (assoc L :leaves (conj (:leaves L) e))))
        {L :leaves I :internals} (reduce one (->Leaves '() '()) E)]
    (doseq [[u v] L]
      (aset D u (- (aget D u) 1))
      (aset D v (- (aget D v) 1)))
    (assoc G :edges I)))

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
  (let [init-G (time (load-graph-2))]
    (dump (count (:edges init-G)))
    (loop [G init-G steps 0]
      (dump steps)
;      (dump (:edges G))
;      (dump (vec (:degrees G))) 
      (if (not (empty? (:edges G)))
        (recur (time (filter-2 G)) (+ 1 steps))
        (println steps)))))
