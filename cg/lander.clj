(ns Player (:gen-class))

(defn- height [surface x y]
  (let [[xa xb ya yb] (for [i [first second] j [first second]] (comp i j))
        cut           (first (filter (fn [p] (<= (xa p) x (xb p))) surface))
        [x1 y1 x2 y2] ((juxt xa ya xb yb) cut)
        k             (float (/ (- y2 y1) (- x2 x1)))
        yp            (+ y1 (* k (- x x1)))]
    (- y yp)))

(def ^:const M 3.711)

(defn- integrate [[h v] p]
  (let [a (- M p)] [(+ h v (* 0.5 a a)) (+ v a)]))

(defn- solve-sq [a b c]
  (let [D (- (* b b) (* 4.0 a c))]
    (if (< D 0)
      nil
      (let [sqrt-D (Math/sqrt D) q (* 2 a)]
        (mapv (fn [op] (/ (op (- b) sqrt-D) q)) [+ -])))))

(defn- thrust [H V P]
  (let [[h v] (reduce integrate [H (- V)] (range P 4))
        a (- M 4)
        [tp tm] (solve-sq (* 0.5 a) v (- h))]
    (binding [*out* *err*]
      (println "HVP:" [H V P] "hv:" [h v] "times:" [tp tm]))
    (if (and tp (< 37 (+ v (* a tp)))) 4 0)))

(defn -main [& args]
  (let [surface (partition 2 1 (partition 2 (repeatedly (* 2 (read)) read)))]
    (binding [*out* *err*] (println "Surface:" surface))
    (loop [hold 0]
      (let [[X Y dx dy fuel rotate power :as game] (vec (repeatedly 7 read))
            h (height surface X Y)]
        (binding [*out* *err*] (println "height:" h "thurst:" power))
        (println (str 0 \space (thrust h dy power)))
      (recur (inc hold))))))
