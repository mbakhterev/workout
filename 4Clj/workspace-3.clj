(def rotate
(fn [r T]
  (letfn [(d [S t]
            (if (S :C)
              (assoc S :R (conj (S :R) t))
              (let [v (R t)
                    h (and (coll? v) (= r (first v)))
                    k (if h :C :L)]
                (assoc S k (conj (S k []) v)))))

          (slice [T] (reduce d {:L [] :R []} T))

          (R [V]
            (cond (not (coll? V)) V
                  (= r (first V)) (concat [r :mark] (rest V))
                  :else (apply concat ((juxt :L :C :R) (slice V)))))
          
          (init-chain [V] (fn [v] (concat V [v])))
          (lift-chain [V f] (fn [v] (f (concat V [v]))))
          ]
    (R T)))
)

(def rotate
(fn [r T]
  (letfn [(d [S t] (let [v (if (S :C) t (R t)) k (if (fn? v) :C :R)] (assoc S k (conj (S k []) v))))

          (chain [V f] (fn [v] (f (concat V (if v [v])))))

          (R [V]
            (cond (not (coll? V)) V
                  (= r (first V)) (chain V identity)
                  :else (let [S (reduce d {} V) f (first (S :C)) r (S :R)] (if f (chain r f) r))))]
    ((R T) nil)))
)

(def tree '(a (b (c (d) (e)) (f (g) (h))) (i (j (k) (l)) (m (n) (o)))))

(def rotate
(fn [r T]
  (->> (tree-seq coll? rest T)
       (filter (fn [t] (some (partial = r) (flatten t))))
       (reduce (fn [R t] (concat t (remove (partial = t) R))))))
)

(rotate 'c '(d x y c (c)))
(rotate 'c tree)
(rotate 'n '(n))

(def rotate (fn [M] (mapv (comp vec reverse) (apply map vector M))))

(def spin (fn [R d]
            (let [l (last R)
                  L (vec (drop-last R))
                  c (= (count l) (-> R first count))]
              (if c
                (conj (rotate R) [d])
                (conj L (conj l d))))))

(def spin (fn [S]
            (loop [R [[(first S)]] Q (rest S)]
              (if (empty? Q)
                R
                (let [[F L] (split-at (count (first R)) Q)] (recur (rotate (conj R (vec F))) L))))))

(def slice (fn [M]
             (let [l (count M)
                   c (if (= 0 (mod l 2)) (comp rotate) identity)
                   n (dec (* 2 l))
                   P (map (fn [x] (take l (iterate (partial map deliver [inc dec]) [0 x]))) (range n))
                   F (map (fn [p] (keep (partial get-in (c M)) p)) P)
                   S (map (fn [s] (let [i (interpose \space s) m (count i) p (repeat (quot (- n m) 2) \space)] (apply str (concat p i p)))) F)]
               S)))

(def sqsq
(fn [s e]
  (let [D (mapcat str (take-while (partial >= e) (iterate (fn [x] (* x x)) s)))
        n (count D)
        l (-> n Math/sqrt Math/ceil int)
        E (concat D (repeat (- (* l l) (count D)) \*))
        rotate (fn [M] (mapv (comp vec reverse) (apply map vector M)))

        spin (fn [S]
               (loop [R [[(first S)]] Q (rest S)]
                 (if (empty? Q) R (let [[F L] (split-at (count (first R)) Q)] (recur (rotate (conj R (vec F))) L)))))

        slice (fn [M]
                (let [c ([(comp rotate rotate) identity] (mod l 2))
                      n (dec (* 2 l))
                      P (map (fn [x] (take l (iterate (partial map deliver [inc dec]) [0 x]))) (range n))
                      F (map (fn [p] (keep (partial get-in (c M)) p)) P)]
                  (map (fn [s] (let [I (interpose \space s) p (repeat (quot (- n (count I)) 2) \space)] (apply str (concat p I p)))) F)))]
    (-> E spin slice)))
)

(def sqsq
(fn [s e]
  (let [D (->> (iterate (fn [x] (* x x)) s) (take-while (partial >= e)) (mapcat str) vec)
        l (-> (count D) Math/sqrt Math/ceil int)
        w (dec (+ l l))
        n (* l l)
        oi (+ l -2 (mod l 2))
        oj (+ l -1)
        I (->> (range) (mapcat (fn [k] (repeat (+ k k) (- (* 2 (mod k 2)) 1)))) (reductions + oi) (take n) vec)
        J (->> (range) (mapcat (fn [k] (repeat (+ k k 1) (- 1 (* 2 (mod k 2)))))) (reductions + oj) (take n) vec)
        M (into {} (for [i (range n)] [((juxt I J) i) (get-in D [i] \*)]))]
  (for [i (range w)] (apply str (for [j (range w)] (M [i j] \space))))))
)

((reduce (fn [R V] (fn [v] (concat V (if-let [w (R v)] [(R v)])))) identity [[:a :b] [:c :d] [:e :f]]) nil)

((reduce (fn [R V] (fn [v] (R (concat V (if v [v]))))) identity [[:a :b] [:c :d] [:e :f]]) [:x :zeta])

(= (-> R last count) (-> R first count))
((juxt (comp count last) (comp count first)) x)

(reduce = (map count ((juxt first last) R)))

(def mine
(fn [m]
  (let [L (mapv (fn [i] (Integer/toBinaryString i)) m)
        l (apply max (map count L))
        M (mapv (fn [i] (vec (concat (repeat (- l (count i)) \0) i))) L)
        rotate (fn [M] (mapv (comp vec reverse) (apply map vector M)))
        born (fn [v] (mapcat identity (keep-indexed (fn [i t] (if (= \1 t) [[:L i 1 1] [:V i 1 1]])) v)))

        grow (fn [T v]
               (concat (born v)
                       (keep (fn [[t p s a]]
                               (let [[i w] (if (= t :L) [p (+ 1 s)] [(dec p) (+ 2 s)])]
                                 (if (and (<= 0 i) (= (repeat w \1) (take w (drop i v))))
                                   [t i w (+ a w)]))) T)))

        R (fn R [T V] (concat T (if-not (empty? V) (R (grow T (first V)) (rest V)))))
        m (apply max (map (fn [t] (t 3)) (mapcat (partial R []) (take 4 (iterate rotate M)))))]
    (if (< 2 m) m)))
)

(def rock [18 7 14 14 6 3])
