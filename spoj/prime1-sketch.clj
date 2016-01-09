(ns spoj.prime1)

(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

(defn sieve [s]
    (cons 
      (first s)
      (lazy-seq (sieve (filter #(not= 0 (mod % (first s))) (rest s))))))

; (time (dorun (take 100 (iterate #(long (inc %)) 2))))

(take 100 (iterate #(long (inc %)) 2))

(dorun (map println (sieve [2 3 4 5 6 7 8 9 10])))

((fn [s] (lazy-seq (filter #(not= 0 (mod % (first s))) s))) [2 3 4 5 6 7 8 9])

(defn sieve [s]
  (cons
    (first s)
    ; (lazy-seq (sieve (filter #(not= 0 (mod % (first s))) (rest s))))))
    (lazy-seq
      (let [t (filter #(not= 0 (mod % (first s))) (rest s))]
        (if (empty? t) nil (sieve t))))))

(defn- primes-1 [n]
  (loop [P [2] i 3]
    (cond
      (> i n) P
      (not (some #(zero? (mod i %)) P)) (recur (conj P i) (inc i))
      :else (recur P (inc i)))))

(defn- sieve-loop [ns]
  (loop [P [2] s ns]
    (if (empty? s)
      P
      (recur (conj P (first s))
             (filterv #(not= 0 (mod % (first s)))) (rest s)))))

(defn primes [n]
  (if (<= n 3)
    [2 3]
    (sieve-loop (range 3 n 2))))

(time (dorun (sieve (take 1000000 (iterate #(long (inc %)) 2)))))

(time (count (primes 100000)))
(time (count (range 3 100000000 2)))

(take-nth 2 (range 3 10 2))

(time
  (doseq [x (range 1 10000)]
    (dorun (map + (range 0 10000) (range 0 10000)))))

(time
  (count (repeat 100 (time (dorun (map + (range 0 10000) (range 0 10000)))))))


(defn- binary-search
  ([N n] (binary-search (vec N) n 0 (dec (count N))))
  ([N n l r]
    (if (< r l) l
      (let [m (+ l (bit-shift-right (- r l) 1)) mth (nth N m)]
        (if (> mth n)
          (recur N n l (dec m))
          (recur N n (inc m) r))))))

(defn- binary-miss [N n]
  (let [idx (binary-search N n)]
    (cond
      (== 0 idx) [0 true]
      (== n (nth N (dec idx))) [(dec idx) false]
      :else [idx true])))

(primes 100)
(binary-search (primes 100) 3)

; Не пойдёт. Необходимо постоянно сортировать :(

(defn- update-steps [n P S]
  (println "updating:" n "primes:" P "steps:" S)
  (loop [new-S  []
         rest-S S
         rest-P P]
    (let [s (first rest-S) p (first rest-P)]
      (if (> s n)
        (vec (concat new-S rest-S))
        (recur
          (conj new-S (+ s p))
          (rest rest-S)
          (rest rest-P))))))

(defn- primes
  ([n] (if (< n 3) [2] (primes [2] [4] 3 n)))
  ([P S i n] ; P - primes, S - steps - следующие шаги в решете
    (if (> i n)
      P 
      (let [[idx prime] (binary-miss S i)]
        (println "checking:" i "steps:" S "index prime:" idx prime)
        (let [next-P  (if prime (conj P i) P)
              next-S  (update-steps i next-P (if prime (conj S (* 2 i)) S))]
          (recur next-P next-S (inc i) n))))))

(primes 20)

(let [[x y] [10 20]] (println x y))

(defmacro primes-gen [] (vec (primes-1 (+ 1 (Math/sqrt 1000000000)))))

(def primes-vec (primes-gen))

(defn sieve-range [M N]
  (let [P (take (+ 1 (Math/sqrt N)) primes-vec) R (range M N)]
    (filter (fn [i] (not (some #(and (not= i %) (zero? (mod i %))) P))) R)))

(time
  (count
    (filterv
      (fn [i] (not (some #(and
                            (not= (long i) (long %))
                            (zero? (mod (long i) (long %))))
                         (take (+ 1 (Math/sqrt (+ 99991 100000))) primes-vec))))
      (range 99991 (+ 99991 100000)))))

(time (count (sieve-range 50 100)))

(defn- bit-position [n] [(quot n 32) (mod n 32)])

(defn- vector-set-bit [^longs V i]
  (let [[pos offset] (bit-position i)]
    (concat
      (take (- pos 1) V)
      [(bit-set (nth V pos) offset)]
      (drop (+ 1 pos) V))))

(let [n (long 1e5) N (+ 1 (quot n 64))]
  (time
    (count
      (map bit-or (repeatedly N #(rand-int n)) (repeatedly N #(rand-int n))))))

(/ (* 6.5 (count prime-vec)) 1000)

let [n     (long 1e5)
      sieve (reduce (partial conj) #{} (repeatedly (* n 10) #(rand-int n)))]
  (time (filter #(not (sieve %)) (range 1 (+ 1 n)))))
