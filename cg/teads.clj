(ns Solution (:gen-class))

(defn- dump [& args] (binding [*out* *err*] (apply println args)))

(defrecord Graph [^longs degrees edges])
(defrecord Leaves [leaves internals])

(letfn [(normalize [[^long u ^long v :as e]] (if (<= u v) (vec e) [v u]))]
  (defn- make-graph ^Graph [edges]
    (let [E edges ; (seq (set (map normalize edges)))
          M (reduce (fn ^long [^long M [^long u ^long v]] (max M u v)) 0 E)
          D (long-array (+ 1 M))]
      (doseq [[u v] E]
        (aset D u (+ 1 (aget D u)))
        (aset D v (+ 1 (aget D v))))
      (->Graph D E))))


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

(defn -main [& args]
  (let [init-G (make-graph (partition 2 (repeatedly (* 2 (read)) read)))]
    (dump (count (:edges init-G)))
    (loop [G init-G steps 0]
      (if (not (empty? (:edges G)))
        (recur (filter-leaves G) (+ 1 steps))
        (println steps)))))
