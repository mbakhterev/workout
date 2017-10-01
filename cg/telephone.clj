(ns Solution (:gen-class))

(defmacro ^:private dump-time [code] `(binding [*out* *err*] (time ~code)))

(defn- dump [& args] (binding [*out* *err*] (apply println args)))

(defn- load-data []
  (seq (into (sorted-set) (repeatedly (let [n (read)] (read-line) n) read-line))))

(defn- n-count ^long [^long offset numbers]
  (let [long-enough (filter (fn [n] (< offset (count n))) numbers)
        splits (partition-by (fn [n] (nth n offset)) long-enough)]
    (apply + (count splits) (map (partial n-count (+ 1 offset)) splits))))

(defn -main [& args]
  (let [N (dump-time (load-data))
        c (dump-time (n-count 0 N))]
    (println c)))
