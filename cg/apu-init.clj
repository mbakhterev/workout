(ns Player (:gen-class))

; Don't let the machines win. You are humanity's last hope...

(defn- search-next [i j M]
  (let [R (drop (+ 1 j) (nth M i))
        C (map #(nth % j nil) (drop (+ 1 i) M))
        dj (first (keep-indexed #(if (= %2 \0) %1) R))
        di (first (keep-indexed #(if (= %2 \0) %1) C))]

    (comment (binding [*out* *err*] (println "i j:" i j "C:" C)))

    (concat [j i]
            (if dj [(+ j dj 1) i] [-1 -1])
            (if di [j (+ i di 1)] [-1 -1]))))

(defn -main [& args]
  (let [[W H] (repeatedly 2 (comp read-string read-line))
        M (repeatedly H read-line)
        D (for [y (range H) x (range W)] (if (= (nth (nth M y) x) \0) (search-next y x M)))]
    ; W: the number of cells on the X axis
    ; Y: the number of cells on the Y axis

    (comment (binding [*out* *err*]
               (println W H)
               (doseq [l M] (println l))
               (doseq [l D] (println l))))

    ; Three coordinates: a node, its right neighbor, its bottom neighbor
    (doseq [r (keep identity D)] (let [[x y x1 y1 x2 y2] r] (println x y x1 y1 x2 y2)))))
