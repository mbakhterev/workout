(ns Solution (:gen-class))

(defn- dump [& args] (binding [*out* *err*] (apply println args)))

(defn -main [& args]
  (let [loss (fn [pairs]
               (let [a (first (first pairs))
                     b (last (last pairs))]
                 (if (> a b) (- b a))))
        periods (partition-by (partial apply >) (partition 2 1 (repeatedly (read) read)))
        x (keep loss periods)]
    (println (apply min x))))
