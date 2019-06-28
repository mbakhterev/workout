(ns faberge)

; Пересчёт высот из массива H в массив HN. H описывает высоты, рассчитанные для
; попыток (от 0 до m) и количества яиц n. HN должен описывать высоты для такого
; же набора попыток и количества яиц n+1. Массивы удобно сделать длиной m+1.
; Функция работает при условии, что n < m

(defn step [n H HN]
  (assert (< (+ 1 n) (count H)))
  
  (aset HN n (biginteger (- (.pow (biginteger 2) n) 1)))
  (doseq [m (range (+ 1 n) (count H))]
    (aset HN m (biginteger (+ 1
                              (aget H (- m 1))
                              (aget HN (- m 1)))))))

(defn zero-h-array [m]
  (let [A (make-array BigInteger (+ 1 m))]
    (doseq [i (range (+ 1 m))] (aset A i (biginteger 0)))
    A))

(defn height [n m]
  (cond ; (>= n m) (- (.pow (biginteger 2) m) 1)
        :else (loop [i 0
                     H (zero-h-array m)
                     HN (zero-h-array m)]
                (println i (vec H))
                (if (= i n)
                  (aget H m)
                  (do (step i H HN)
                      (recur (+ 1 i) HN H))))))
