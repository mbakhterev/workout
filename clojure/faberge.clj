(ns faberge)

; Пересчёт высот из массива H в массив HN. H описывает высоты, рассчитанные для
; попыток (от 0 до m) и количества яиц n. HN должен описывать высоты для такого
; же набора попыток и количества яиц n+1. Массивы удобно сделать длиной m+1.
; Функция работает при условии, что n < m

; (defn- expt-2 [n] (.subtract (.shiftLeft BigInteger/ONE n) BigInteger/ONE))
(defn- add-1 [p q] (.add p (.add q BigInteger/ONE)))

(defn- expt-2 [n]
  (loop [r 1N m 2N k n]
    (cond (zero? k) (- r 1N)
          (zero? (bit-and k 1)) (recur r (* m m) (bit-shift-right k 1))
          :else (recur (* r m) (* m m) (bit-shift-right k 1)))))

(defn- add-1 [p q] (+ 1N p q))

(defn- zeroes [m]
  (let [A (make-array clojure.lang.BigInt (+ 1 m))]
    (doseq [i (range (+ 1 m))] (aset A i 0N))
    A))

(defn step-old [n H]
  (assert (< (+ 1 n) (count H)))

  (loop [p (expt-2 n)
         q (aget H n)
         i n]
    (aset H i p)       
    (if (= (+ i 1) (count H))
      H
      (recur (add-1 p q) (aget H (+ 1 i)) (+ 1 i)))))

(defn height-old [n m]
  (loop [i 0 H (zeroes m)]
    (if (= i n)
      (aget H m)
      (recur (+ 1 i) (step i H)))))
