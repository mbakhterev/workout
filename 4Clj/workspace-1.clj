(fn F [S] (reduce (fn [R s] (concat R (if-not (coll? s) [s] (let [t (F s)] (if (= t s) [s] t))))) [] S))
; (defn- simple-next [L R]
;   (loop [l L r R n '()]
;     (if (empty? l)
;       n
;       (let [fl (first l) fr (first r)]
;         (if (>= (int fl) (int fr))
;           (recur (rest l) (rest r) (cons fl n))
;           (concat (reverse (rest l)) [fr] n))))))
; 
; (defn- renum [s] (reduce (fn [R d] (+ (* R 10) (- (int d) (int \0)))) 0 s))

(defn- renum [s] (read-string (apply str s)))

(defn- cutcmp [f x y] (f (compare x y) 0))

(defn- numcut [k]
  (let [S (str k)
        n (count S)
        l (+ (quot n 2) (mod n 2))
        r (quot n 2)
        L (reverse (take l S))
        R (drop r S)]
    [l r (vec L) (vec R)]))

(defn- simple-next [l r L]
  (concat (reverse L) (if (= l r ) L (rest L))))

(defn- complex-next [l r L]
  (loop [[c & C :as Q] L R [] hit false]
    (if (empty? Q)
      (simple-next l r (if hit R (conj R 1)))
      (if hit
        (recur C (conj R c) hit)
        (let [h (not= c \9)]
          (recur C (conj R (if h ((comp char inc int) c) 0)) h))))))

(defn- p-next [k]
  (let [[l r L R] (numcut k)]
    (list L R k (< 0 (compare (str L) (str R))))
    (renum ((if (cutcmp > L R) simple-next complex-next) l r L))))

(defn- go [k]
  (let [[l r L R] (numcut k)]
    (iterate p-next (if (cutcmp = L R) k (p-next k)))))

(def go-fn
(fn [k]
  (letfn [(to-num [s] (read-string (apply str s)))

          (split-num [k]
            (let [S (str k) n (count S) r (mod n 2)]
              [(= 0 r) (take (+ (quot n 2) r) S)]))

          (simple-next [[even L]]
            (let [R (reverse L)]
              (concat L (if even R (rest R)))))

          (smart-next [[even L]]
            (let [R (str (+ 1 (to-num L)))]
              (simple-next
                (if (= (count L) (count R))
                  [even R]
                  [(not even) (if even R (drop-last R))]))))

          (p-next [k]
            (let [S (split-num k) n (to-num (simple-next S))]
              (if (< k n) n (to-num (smart-next S)))))]

    (iterate p-next (if (= 0 k) 0 (p-next (dec k))))))
)

(numcut 12344321)
(read-string (str \1 \2 \3))
(p-next 1234567)
(p-next 12341234)
(p-next 12350001)
(p-next 123456008796)
(p-next 1234569008796)
(p-next 192)
(p-next 191)
(p-next 1991)
(p-next 1234990456)
(renum "1234")
(concat [1] ((take 2) [1 2 3 4]))

(compare [1 2 3 4] [1 2 3 4])

(compare [\1 \2] [\3 \4])

(->> 1 (fn [x] (+ x 2)))
((comp char inc int) \8)

(take 26 (go-fn 0))

(clojure.set/difference
  (set (take 199 (go-fn 0))) 
  (set (map #(first (go-fn %)) (range 0 10000))))

(take 199 (go-fn 0))

(rest [1 2 3 4])

(apply (fn [x y z] (+ x y z)) 0 [2 3])

(compare [1 2 3] [4 5 6])

(next [1 2 3])

(num [1 2 3])

(doc num)

(compare '(1 2 3) '(4 5 6))

(+ 100000000000000000000N 1)

o-fn 1234567)

(take 2 (go-fn 9999999999))
(def go
(fn M
  ([f] (M f 0 0))
  ([f m n]
    (letfn [(R [i] (lazy-seq (cons (C i n) (R (inc i)))))
            (C [i j] (lazy-seq (cons (f i j) (C i (inc j)))))] (R m)))
  ([f m n s t] (map (partial take t) (take s (M f m n)))))
)
(def go-long
(fn [S]
  (letfn [(R [[x & [y & T :as Q] :as S]]
            (if (empty? S)
              nil
              (lazy-cat
                (if (or (empty? Q) (< x y)) [x] [x :v])
                (R Q))))]
    (let [T (partition-by number? (R S)) m (apply max (map count T))]
      (if (= 1 m) '() (first (filter (fn [x] (= m (count x))) T))))))
)

(def go-long
(fn [S] (or (first (filter (partial apply <) (mapcat (fn [l] (partition l 1 S)) (range (count S) 1 -1)))) []))
)

(def tic-tac
(fn [B]
  (let [R (range 3)
        p (fn [s t] (map vector s t))
        V (lazy-cat [(p R R)]
                    [(p (reverse R) R)]
                    (map (fn [r] (p R (repeat 3 r))) R)
                    (map (fn [r] (p (repeat 3 r) R)) R))
        S (map (fn [v] (map (fn [[i j]] (nth (nth B i) j)) v)) V)]
    (some {[:x :x :x] :x [:o :o :o] :o} S)))
)

(def tic-tac
(fn [B]
  (let [R (range 3)
        V (lazy-cat [R (reverse R)] (map (fn [r] (repeat 3 r)) R))
        L (lazy-cat B (map (fn [v] (map nth B v)) V))]
    (some {(repeat 3 :x) :x (repeat 3 :o) :o} L))))

;         p (fn [s t] (map vector s t))
;                     [(p (reverse R) R)]
;                     (map (fn [r] (p R (repeat 3 r))) R)
;                     (map (fn [r] (p (repeat 3 r) R)) R))
;         S (map (fn [v] (map (fn [[i j]] (nth (nth B i) j)) v)) V)]
;     (some {[:x :x :x] :x [:o :o :o] :o} S)))
; )

(def board [[:e :x :o] [:e :x :o] [:e :x :o]])

(take-nth 2 (apply concat board))

(def board [[:x :e :o] [:x :o :e] [:o :e :x]])

(def read-rome
(fn R [[x & [y & Q :as T] :as S]]
  (let [M {"M" 1000 "CM" 900 "D" 500 "CD" 400 "C" 100 "XC" 90 "L" 50 "XL" 40 "X" 10 "IX" 9 "V" 5 "IV" 4 "I" 1}
        n (M (str x y))
        m (M (str x))]
    (if (empty? S) 0 (if n (+ n (R Q)) (+ m (R T))))))
)

(def read-rome
(fn [s] (reduce + (map {"M" 1000 "CM" 900 "D" 500 "CD" 400 "C" 100 "XC" 90 "L" 50 "XL" 40 "X" 10 "IX" 9 "V" 5 "IV" 4 "I" 1} (re-seq #"C[MD]|X[CL]|I[XV]|." s))))
)

(def tr-path
(fn [s] (reductions (fn [P p] (let [L (map + p (cons (first P) P)) R (map + p (conj P (last P)))] (mapv min L R))) s))
)

(def triangle '([3] [2 4] [1 9 3] [9 9 2 4] [4 6 6 7 8] [5 7 3 5 1 4]))

(def tr-close
(fn [r]
  (let [R (reduce (fn [R [t f]] (assoc R f (conj (R f []) t))) {} r)
        g (fn [k]
            (loop [[q & Q :as N] (R k) S #{}]
              (if (empty? N) S (recur (concat Q (if-let [t (R q)] (if-not (S q) t))) (conj S q)))))]
    (set (mapcat (fn [k] (map vector (g k) (repeat k))) (keys R)))))
)

(def tr-close
(fn [r]
  (let [R (fn [k] (map first ((group-by second r) k)))
        g (fn [k]
            (loop [[q & Q :as N] (R k) S #{}]
              (if (empty? N) S (recur (concat Q (if-let [t (R q)] (if-not (S q) t))) (conj S q)))))]
    (set (mapcat (fn [k] (map (partial vector k) (g k))) (map second r)))))
)

(def tr-close
(fn [S]
  (let [R (group-by second S)
        T (fn [[t f]] (map (fn [[a b]] (vector a f)) (R t)))
        F (fn F [e] (let [t (T e)] (concat [e] t (mapcat F t))))]
    (set (mapcat F S))))
)


(def initial #{[8 4] [9 3] [4 2] [27 9]})

(into #{} [1 2 3])

((group-by second initial) 9)

(tr-close initial)
