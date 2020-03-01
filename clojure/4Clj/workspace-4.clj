(def latin
(fn [V]
  (let [M (count V)
        N (apply max (map count V))]
    (letfn [(row-slicer [v w]
              (let [l (count v)
                    s (map vec (partition w 1 v))]
                (fn [i]
                  (let [k (max 0 (- i (- N l)))
                        m (+ (- i k) 1)]
                    (take m (drop k s))))))
            
            (slicers [v] (into {} (map (fn [w] [w (row-slicer v w)]) (range 2 (+ 1 (min M N (count v)))))))

            (born [S h]
              (mapcat identity
                      (for [w (range 2 (+ 1 (min h (apply max (cons 0 (keys S))))))
                            i (range 0 (+ 1 (- N w)))]
                        (map (fn [s] [i [s]]) ((S w) i)))))

            (grow [S Q]
              (reduce (fn [R [i q]] (let [w (count (first q))
                                          k (if (= 1 (- w (count q))) :R :C)
                                          s (S w)
                                          r (map (comp (partial vector i) (partial conj q)) (if s (s i)))]
                                      (assoc R k (into (R k) r))))
                      {:R [] :C []} Q))

            (R [T V]
              (if-not (empty? V)
                (let [S (slicers (first V))
                      {r :R c :C} (grow S T)]
                  (lazy-cat r (R (concat c (born S (count V))) (rest V))))))
            
            (latin? [Q]
              (let [q (set (first Q))]
                (and (= (count q) (count Q))
                     (every? (comp (partial = q) set) (concat Q (apply map vector Q))))))]

    (frequencies (map count (set (filter latin? (map second (R [] V)))))))))
)

(def data [[3 1 2] [1 2 3 1 3 4] [2 3 1 3]])

(def data [[1] [1 2 1 2] [2 1 2 1] [1 2 1 2] []])

(latin data)

(def veitch
(fn [F]
  (letfn [(sym-diff [s t] (into (apply disj s t) (apply disj t s)))

          (find-pair-1 [C]
            (comment "FP:" C "SD:" (sym-diff (first C) (second C)))
            (if-not (empty? C)
              (let [Q (rest C)
                    c (first C)
                    p (some (fn [v] (let [d (sym-diff c v)] (comment "FP:" c v d) (if (apply = (map clojure.string/lower-case d)) [c v d]))) Q)]
                (if p p (find-pair Q)))))

          (pred [{D :d}] (= 2 (count D) (* 2 (count (set (map clojure.string/lower-case D))))))

          (find-pair [C] (comment "FP:" C) (sort-by (comp - count :d) (filter pred (for [s C t C] {:a s :b t :d (sym-diff s t)}))))
          
          (R-1 [C]
            (comment "pre find-pair")
            (let [[c v d] (find-pair C)]
              (comment "R: CVD:" c v d)
              (if-not d
                C
                (do (comment (apply disj c d) (apply disj v d))
                    (R (into (disj C c v) [(apply disj c d) (apply disj v d)]))))))
          
          (R [C] (println "C:" C "FP:" (first (find-pair C)))
            (let [{a :a b :b d :d} (first (find-pair C))] (comment C a b d (disj C a b) (apply disj a d)) (if-not d C (R (into (disj C a b) [(apply disj a d)])))))]

    (R F)))
)

(def veitch
(fn [F]
  (letfn [(sym-diff [s t] (into (apply disj s t) (apply disj t s)))
          (gen-diff [C] (if-not (empty? C) (let [q (first C) Q (rest C)] (concat (map (fn [f] {:a q :b f :d (sym-diff q f)}) Q) (gen-diff Q)))))
          (pred [{D :d}] (= 2 (count D) (* 2 (count (set (map clojure.string/lower-case D))))))
          (clue [C] (let [D (filter pred (gen-diff C))] (if (empty? D) [C] (let [{a :a b :b d :d} (first D)] (cons (R (conj (disj C a b) (apply disj a d)))) D))))]
    (clue F))
)

(def veitch
(fn [F]
  (letfn [(clue [F] )]))
)

(def fv [#{#{'a 'B 'C 'd} #{'A 'b 'c 'd} #{'A 'b 'c 'D} #{'A 'b 'C 'd} #{'A 'b 'C 'D} #{'A 'B 'c 'd} #{'A 'B 'c 'D} #{'A 'B 'C 'd}}
         #{#{'a 'b 'c} #{'a 'B 'c} #{'a 'b 'C} #{'a 'B 'C}}
         #{#{'A 'B 'C 'D} #{'A 'B 'C 'd}}
         #{#{'a 'b 'c 'd} #{'a 'B 'c 'd} #{'a 'b 'c 'D} #{'a 'B 'c 'D} #{'A 'B 'C 'd} #{'A 'B 'C 'D} #{'A 'b 'C 'd} #{'A 'b 'C 'D}}
         #{#{'a 'b 'c 'd} #{'A 'b 'c 'd} #{'a 'B 'c 'D} #{'A 'B 'c 'D} #{'a 'B 'C 'D} #{'A 'B 'C 'D} #{'a 'b 'C 'd} #{'A 'b 'C 'd}}
         #{#{'a 'B 'c 'd} #{'A 'B 'c 'D} #{'A 'b 'C 'D} #{'a 'b 'c 'D} #{'a 'B 'C 'D} #{'A 'B 'C 'd}}])

(def trouble (second fv))

(veitch trouble)
