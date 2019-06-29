(ns faberge)

; Пересчёт высот из массива H в массив HN. H описывает высоты, рассчитанные для
; попыток (от 0 до m) и количества яиц n. HN должен описывать высоты для такого
; же набора попыток и количества яиц n+1. Массивы удобно сделать длиной m+1.
; Функция работает при условии, что n < m

(defn- expt-2 [n] (.subtract (.shiftLeft BigInteger/ONE n) BigInteger/ONE))
(defn- add-1 [p q] (.add p (.add q BigInteger/ONE)))

(defn- big-zeroes [m]
  (let [A (make-array BigInteger (+ 1 m))]
    (doseq [i (range (+ 1 m))] (aset A i BigInteger/ZERO))
    A))

(defn step [n H]
  (assert (< (+ 1 n) (count H)))

  (loop [p (expt-2 n)
         q (aget H n)
         i n]
    (aset H i p)       
    (if (= (+ i 1) (count H))
      H
      (recur (add-1 p q) (aget H (+ 1 i)) (+ 1 i)))))

(defn step-old [n H HN]
  (assert (< (+ 1 n) (count H)))
  
  (aset HN n (biginteger (- (.pow (biginteger 2) n) 1)))

  (doseq [m (range (+ 1 n) (count H))]
    (aset HN m (biginteger (+ 1
                              (aget H (- m 1))
                              (aget HN (- m 1)))))))

(defn height-old [n m]
  (cond ; (>= n m) (- (.pow (biginteger 2) m) 1)
        :else (loop [i 0
                     H (big-zeroes m)
                     HN (big-zeroes m)]
                ; (println i (vec H))
                (if (= i n)
                  (aget H m)
                  (do (step-old i H HN)
                      (recur (+ 1 i) HN H))))))

(defn height [n m]
  (loop [i 0 H (big-zeroes m)]
    (if (= i n)
      (aget H m)
      (recur (+ 1 i) (step i H)))))
