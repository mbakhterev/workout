(def chain
(fn [W]
  (let [l (fn [x y] (= 1 (count (filter identity (map not= x y)))))
        i (fn [x y]
            (let [[t e] (if (apply > (map count [x y])) [x y] [y x])]
              ((set (for [i (range (count t))] (let [[u w] (split-at i t)] (apply str (concat u (drop 1 w)))))) e)))
        L (filter (fn [[u w]] (or (l u w) (i u w))) (for [x W y W] [x y]))
        R (reduce (fn [S [f t]] (assoc S f (conj (S f []) t))) {} L)
        F (fn F [w S] (if (= S W) true (some (fn [i] (F i (conj S i))) (filter (comp not S) (R w)))))]
     (or (some identity (map (fn [w] (F w #{w})) W)) false)))
)

(def chain
(fn [W]
  (let [nxt (fn [x y]
              (loop [[a & A :as u] (seq x) [b & B :as v] (seq y)] 
                (cond (= a b) (recur A B)
                      (or (= u B)
                          (= v A)
                          (= A B)) true
                      :else false)))
        R (fn [w S] (filter (partial nxt w) S))
        F (fn F [w S]
            (if (empty? S) true (boolean (some (fn [i] (F i (disj S i))) (R w S)))))]
    (boolean (some (fn [w] (F w (disj W w))) W))))
)


(def i-alone
  (fn [x y]
    (let [[t e] (if (apply > (map count [x y])) [x y] [y x])
          T (set (for [i (range (count t))] (let [[u w] (split-at i t)] (apply str (concat u (drop 1 w))))))] 
      (T e))))

(def words #{"hat" "coat" "dog" "cat" "oat" "cot" "hot" "hog"})

(def words #{"cot" "hot" "bat" "fat"})

(def graph
(fn [E]
  (let [G (reduce (fn [R [a b]] (assoc R a (conj (R a #{a}) b) b (conj (R b #{b}) a))) {} E)
        C (loop [R (vals G)]
            (let [D (map (fn [V] (mapcat (fn [v] (apply disj (G v) V)) V)) R)]
              (if (every? empty? D) R (recur (map (fn [r d] (into r d)) R D)))))]
    (boolean (every? (fn [c] (= (count c) (count G))) C))))
)

(def graph
(fn [E]
  (let [G (reduce (fn [R [a b]] (assoc R a (conj (R a #{a}) b) b (conj (R b #{b}) a))) {} E)
        C (loop [R #{(first (keys G))}]
            (let [D (mapcat (fn [v] (apply disj (G v) R)) R)]
              (if (empty? D) R (recur (into R D)))))]
    (= (count G) (count C))))
)

(def graph-input #{[1 2] [2 3] [3 1] [4 5] [5 6] [6 4] [6 3]})

(def life
(fn [B]
  (let [N (count B)
        M (count (first B))
        O (fn [i j] 
            (filter (fn [[r c]] (and (< -1 r N) (< -1 c M)))
                    (concat [[i (dec j)] [i (inc j)]] (for [di [-1 1] dj [-1 0 1]] [(+ i di) (+ j dj)]))))
        L (fn [i j] (reduce (fn [R [r c]] (+ R ({\# 1 \space 0} (nth (nth B r) c)))) 0 (O i j)))]
    (for [i (range N)]
      (apply str
             (for [j (range M)] 
               (let [l (L i j)] 
                 (if (= \# (nth (nth B i) j)) (if (or (< l 2) (< 3 l)) \space \#) (if (= l 3) \# \space))))))))
)

(def life
(fn [B]
  (let [N (count B)
        M (count (first B))
        O (fn [i j] (for [di [-1 0 1] dj [-1 0 1] :when (not= 0 di dj)] [(+ i di) (+ j dj)]))
        L (fn [i j] (count (filter #{\#} (map (partial get-in B) (O i j)))))]
    (for [i (range N)]
      (apply str (for [j (range M)] (condp = (L i j) 2 (get-in B [i j]) 3 \# \space))))))
)
     
(def board ["      "  " ##   " " ##   " "   ## " "   ## " "      "])

(def maze
(fn [s e]
  (loop [[[v l] & Q] [[s 1]]]
    (if (= v e) l (recur (concat Q (for [i (filter integer? ((juxt * + /) v 2))] [i (inc l)]))))))
)

(def tour
(fn [E]
  (let [G (reduce (fn [R [a b]] (assoc R a (conj (R a []) b) b (conj (R b []) a))) {} E)
        N (keys G)
        C (loop [R #{(first N)}]
            (let [D (mapcat (fn [v] (apply disj (set (G v)) R)) R)]
              (if (empty? D) R (recur (into R D)))))
        F (remove (comp even? count) (vals G))]; [G C (keys G) F])))
    (and (= C (set N)) (<= (count F) 2))))
)

(tour [[:a :b]])

(tour #{[1 2] [2 3] [3 1] [4 5] [5 6] [6 4] [3 4]})

(tour #{[:a :b] [:b :c] [:c :d] [:x :y] [:d :a] [:b :e] [:x :a]})

(tour [[:a :b] [:a :b] [:a :c] [:c :a] [:a :d] [:b :d] [:c :d]])

(def levenshtain
(fn [x y]
  (let [u (vec x)
        v (vec y)
        M (count u)
        N (count v)
        R (fn [P m]
            (loop [D [m] n 1]
              (if (< N n)
                D
                (let [dm (dec m)
                      dn (dec n)
                      c (if (= (u dm) (v dn)) 0 1)
                      v (min (inc (P n))
                             (inc (last D))
                             (+ (P dn) c))]
                  (recur (conj D v) (inc n))))))]
    (loop [D (vec (range (inc N))) m 1]
      (if (< M m) (last D) (recur (R D m) (inc m))))))
)

(def mouse
(fn [B]
  (let [M (count B)
        N (count (first B))
        m (first (filter (comp (partial = \M) (partial get-in B)) (for [i (range M) j (range N)] [i j])))
        O (fn [[i j]] (map (fn [k l] [(+ k i) (+ l j)]) [-1 1 0 0] [0 0 1 -1]))]
    (loop [[q & Q :as P] [m] S #{m}]
      (cond (empty? P) false
            (= \C (get-in B q)) true
            :else (let [n (filter (fn [p] (and (#{\C \space} (get-in B p)) (not (S p)))) (O q))] 
                    (recur (concat Q n) (into S n)))))))
)

(def board ["C# # # #" "        " "# # # # " "        " " # # # #" "        " "# # # #M"])

(get-in "board" [1])

(def tic-tac
(fn [k B]
  (let [A (map-indexed (fn [i r] (map-indexed (fn [j v] [v [i j]]) r)) B)
        R (range 3)
        C (lazy-cat [R (reverse R)] (map (fn [r] (repeat 3 r)) R))
        S (lazy-cat A (map (fn [c] (map nth A c)) C))]
    (set (map second (mapcat (fn [l] (if (= 2 (count (filter (comp #{k} first) l))) (filter (comp #{:e} first) l))) S)))))
)

(def board [[:o :e :e] [:o :x :o] [:x :x :e]])

(def dance
(fn [& X]
  (reify clojure.lang.Seqable
    (toString [this] (clojure.string/join ", " (sort X)))
    (seq [this] (reduce (fn [R x] (concat R (if-not ((set R) x) (list x)))) '() X))))
)

(def dance
(fn [& X]
  (reify clojure.lang.Seqable
    (toString [this] (clojure.string/join ", " (sort X)))
    (seq [this] (seq (distinct X)))))
)

(def cross
(fn [w B]
  (let [l clojure.string/lower-case
        r clojure.string/replace
        H (map (fn [s] (l (r s " " ""))) B)
        V (apply map str H)
        R (mapcat (fn [s] (map (fn [p] (re-pattern (r p "_" "."))) (re-seq #"[^#]+" s))) (concat H V))]
    (boolean (some (fn [r] (re-matches r w)) R))))
)

(def cross
(fn [w B]
  (let [l clojure.string/lower-case
        r clojure.string/replace
        H (map #(l (.replace % " " "")) B)
        V (apply map str H)
        R (mapcat (fn [s] (map #(re-pattern (.replace % "_" ".")) (re-seq #"[^#]+" s))) (concat H V))]
    (boolean (some #(re-matches % w) R))))
)

(def board ["_ _ _ # j o y" "_ _ o _ _ _ _" "_ _ f _ # _ _"])

(def quine (list (fn [x] (str x x)) "(fn [x] (str x x))"))

(def pocker
(fn [h]
  (let [d (fn [c] (apply (fn [s r] {:S s :R ({\T 8 \J 9 \Q 10 \K 11 \A 12} r (- (int r) 50))}) c))
        H (map d h)
        R (sort-by :R H)
        S (sort-by :S H)
        pS (partition-by :S S)
        pR (partition-by :R R)
        f (fn [n] (comp (partial = n) count))
        cf (fn [n P] (count (filter (f n) P)))
        ri (fn [C]
             (let [D (sort (map :R C))
                   E (sort (map (fn [c] ({12 -1} (:R c) (:R c))) C))
                   m (apply min D)
                   n (apply min E)
                   l (count C)]
               (or (= D (range m (+ m l))) (= E (range n (+ n l))))))]
    (cond
      (and (= 1 (count pS)) (ri H)) :straight-flush
      (= 1 (cf 4 pR)) :four-of-a-kind
      (= 2 (count pR)) :full-house
      (= 1 (count pS)) :flush
      (ri H) :straight
      (= 1 (cf 3 pR)) :three-of-a-kind
      (= 2 (cf 2 pR)) :two-pair
      (= 1 (cf 2 pR)) :pair
      :else :high-card)))
)

(def pocker
(fn [C]
  (let [S (map first C)
        R (map second C)
        F (-> R frequencies vals sort)
        fl (apply = S)
        st (boolean ((set (map set (partition 5 1 "A23456789TJQKA"))) (set R)))]
    (cond
      (and st fl) :straight-flush
      (= F [1 4]) :four-of-a-kind
      (= F [2 3]) :full-house
      fl :flush
      st :straight
      (= F [1 1 3]) :three-of-a-kind
      (= F [1 2 2]) :two-pair
      (= F [1 1 1 2]) :pair
      :else :high-card)))
)

(def dfa
(fn [{s :start F :accepts T :transitions}]
  (letfn [(R [[q & Q :as L]]
            (if-not (empty? L)
              (let [[r v] q N (map (fn [[c t]] [t (str v c)]) (T r))] (lazy-cat [q] (R (lazy-cat Q N))))))]
    (map second (filter (comp F first) (R [[s ""]])))))
)

(def dfa
(fn [{s :start F :accepts T :transitions}]
  (letfn [(R [Q]
            (if-not (empty? Q)
              (lazy-cat Q (R (mapcat (fn [[r v]] (map (fn [[c t]] [t (str v c)]) (T r))) Q)))))]
    (map second (filter (comp F first) (R [[s ""]])))))
)

(def A '{:states #{q0 q1 q2 q3}
         :alphabet #{a b c}
         :start q0
         :accepts #{q1 q2 q3}
         :transitions {q0 {a q1}
                       q1 {b q2}
                       q2 {c q3}}})

(def reversi
(fn [board c]
  (let [D (range -1 2)
        B (partial get-in board)
        P (fn [i j] (for [di D dj D :when (not= 0 di dj)] (take-while (comp #{'b 'w} B) (drop 1 (iterate (partial map + [di dj]) [i j])))))
        S (fn [i j] (mapcat (fn [p] (let [e (take-while (comp not #{c} B) p)] (if (not= e p) e))) (P i j)))]
   (into {} (for [i (-> board count range) j (-> board first count range) :let [f (S i j)] :when (and (= 'e (B [i j])) (not-empty f))] [[i j] (set f)]))))
)

(def board '[[e e e e] [e w b e] [w w w e] [e e e e]])

( def rotate
(fn [r T]
  (letfn [(R [T] (if (= r (first T)) (concat [r :mark] T) (do (println T) (apply concat ((juxt :L (comp vector :C) :R) (split T))))))
          (split [V]
            (reduce (fn [S t]
                      (let [T (R t) h (= r (first t)) k (if (S :C) :R (if h :C :L))]
                        (assoc S k ((if (= k :C) identity (partial conj (S k []))) T))))
                    {} V))] 
    (split T)))
)

(some identity [nil nil 1 nil 2])

(def tree '(a (b (c (d) (e)) (f (g) (h))) c (i (j (k) (l)) (m (n) (o)))))

(rotate 'c tree)
