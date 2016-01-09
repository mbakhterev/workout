(def quine
(fn [F]
  (letfn [(sym-diff [s t] (into (apply disj s t) (apply disj t s)))
          (grey [s t]
            (let [d (sym-diff s t)]
              (if (and (= 2 (count d)) (apply = (map clojure.string/lower-case d)))
                {:a s :b t :d d})))

          (pairs [C] (if-not (empty? C) (lazy-cat (map (partial vector (first C)) (rest C)) (pairs (rest C)))))

          (primes [C]
            (if-not (empty? C)
              (let [P (keep (partial apply grey) (pairs C))]
                (into (reduce (fn [R {a :a b :b}] (disj R a b)) C P) (primes (set (map (fn [{a :a d :d}] (apply disj a d)) P)))))))

          (measure [C] (reduce (fn [R c] (+ R (count c))) 0 C))

          (R [C I]
            (if (empty? I) [C] (mapcat (fn [i] (R (conj C i) (rest I))) (first I))))]

    (let [P (primes F)
          I (map (fn [t] (filter (partial clojure.set/superset? t) P)) F)]
      (first (sort-by measure (R #{} I))))))
)

(def fv [#{#{'a 'B 'C 'd} #{'A 'b 'c 'd} #{'A 'b 'c 'D} #{'A 'b 'C 'd} #{'A 'b 'C 'D} #{'A 'B 'c 'd} #{'A 'B 'c 'D} #{'A 'B 'C 'd}}
         #{#{'a 'b 'c} #{'a 'B 'c} #{'a 'b 'C} #{'a 'B 'C}}
         #{#{'A 'B 'C 'D} #{'A 'B 'C 'd}}
         #{#{'a 'b 'c 'd} #{'a 'B 'c 'd} #{'a 'b 'c 'D} #{'a 'B 'c 'D} #{'A 'B 'C 'd} #{'A 'B 'C 'D} #{'A 'b 'C 'd} #{'A 'b 'C 'D}}
         #{#{'a 'b 'c 'd} #{'A 'b 'c 'd} #{'a 'B 'c 'D} #{'A 'B 'c 'D} #{'a 'B 'C 'D} #{'A 'B 'C 'D} #{'a 'b 'C 'd} #{'A 'b 'C 'd}}
         #{#{'a 'B 'c 'd} #{'A 'B 'c 'D} #{'A 'b 'C 'D} #{'a 'b 'c 'D} #{'a 'B 'C 'D} #{'A 'B 'C 'd}}])
