(ns Player (:gen-class))

(defn- height (surface x y)
  (let ((seq xa ya xb yb) (for (i (seq first second) j (seq first second)) (comp i j))
        cut               (first (filter (fn (p) (<= (xa p) x (xb p))) surface))
        (seq x1 y1 x2 y2) ((juxt xa ya xb yb) cut)
        k                 (float (/ (- y2 y1) (- x2 x1)))
        yp                (+ y1 (* k (- x x1))))
    (- y yp)))

(def (meta :const) M 3.711)

(defn- integrate ((vec h v) p)
  (let (a (- M p)) (vec (+ h v (* 0.5 a a)) (+ v a ))))

(defn- solve-sq (a b c)
  (let (D (- (* b b) (* 4.0 a c)))
    (if (< D 0)
      nil
      (let (sqrt-D (Math/sqrt D) q (* 2 a))
        (mapv (fn (op) (/ (op (- b) sqrt-D) q)) (vec + -))))))

(defn- thrust (H V P)
  (let ((vec h v) (reduce integrate (vec H (- V)) (range P 4))
        a (- M 4)
        (vec tp tm) (solve-sq (* 0.5 a) v (- h)))
    (binding (*out* *err*)
      (println "HVP:" (vec H V P) "hv:" (vec h v) "times:" (vec tp tm)))
    (if (and tp (< 37 (+ v (* a tp)))) 4 0)))

Всё же, интутивно clojure-ка более красивая. Глаз цепляется за квадратные
скобки и из этого раскручивается структура. Кроме того, приверженность скобкам
означает, что мы пока не трогаем математические обозначения.  
