(ns faberge)

(defn- step [k H] (vec (reductions (partial + 1N) k (next H))))

(defn height [n m]
  (binding [*out* *err*] (println n m))
  (if (>= n m)
    (bigint (.subtract (.shiftLeft BigInteger/ONE m) BigInteger/ONE))
    (loop [i 0 k 1N H (vec (repeat (- (+ m 1) n) 0N))]
      ; (println i (map (partial format "%5d") (map long (map - (rest H) H))))
      (if (= i n)
        (last H)
        (recur (+ 1 i) (+ (* 2 k) 1) (step k H))))))
