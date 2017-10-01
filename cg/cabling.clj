(ns Solution (:gen-class))

(defmacro ^:private dump-times [code] `(binding [*out* *err*] (time ~code)))

(defn- dump [& args] (binding [*out* *err*] (apply println args)))

(defrecord Input [^long x-max ^long x-min Y ^long N])

(defn- load-data []
  (let [s (java.util.Scanner. *in*)
        N (.nextInt s)]
    (loop [n N x-max Long/MIN_VALUE x-min Long/MAX_VALUE Y (transient [])]
      (if (zero? n)
        (->Input x-max x-min (persistent! Y) N)
        (let [x (.nextInt s)
              y (.nextInt s)]
          (recur (- n 1) (max x x-max) (min x x-min) (conj! Y y)))))))

(defn -main [& args]
  (let [{x-max :x-max x-min :x-min Y-raw :Y N :N} (dump-times (load-data))
        m (quot N 2)
        Y (sort Y-raw)
        M (nth Y m)
        S (dump-times (reduce (fn ^long [^long L ^long y] (+ L (Math/abs ^long (- M y))))
                              (- x-max x-min)
                              Y))]
    (dump "x-min x-max:" x-min x-max "N:" N "m-pos:" m "M:" M "S:" S)
    (comment (dump "Y-raw:" Y-raw))
    (comment (dump "Y:" Y))
    (println S)))


