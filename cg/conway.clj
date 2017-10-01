(ns Solution (:gen-class))

(defn- dump [& args] (binding [*out* *err*] (apply println args)))

(defn- encode [input]
  (mapcat (fn [i] (list (count i) (first i))) (partition-by identity input)))

(defn -main [& args]
  (let [R (read)
        L (read)]
    (dump R L)
    (dump (encode (list R)))
    (println (apply str (interpose \space (nth (iterate encode (list R)) (- L 1)))))))
