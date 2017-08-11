(ns roots)

(defrecord Roots [^double left ^double right])

(defn solve-square-equation [^double a ^double b ^double c]
  (if (zero? a)
    (if (not (zero? b))
      (let [t (/ (- c) b)] (->Roots t t)))
    (let [D (- (* b b) (* 4.0 a c))]
      (if (<= 0.0 D)
        (let [D-sqrt (Math/sqrt D)
              a-rcpr (/ (* 2.0 a))
              tp     (* (+ (- b) D-sqrt) a-rcpr)
              tm     (* (- (- b) D-sqrt) a-rcpr)]
          (->Roots (min tp tm) (max tp tm)))))))

(defn square-extremum [^double a ^double b ^double c ^double t]
  (if (zero? a)
    (+ (* b t) c)
    (let [tx (/ (- b) a 2.0)]
      (if (<= 0.0 tx t)
        (+ (* a tx tx) (* b tx) c)
        (if (< t tx)
          (+ (* a t t) (* b t) c)
          c)))))

; Рассчёт времени пересечения траектории с ускорением (ax ay) начальной скоростью (vx vy) и
; положением (x y) и прямой проходящей через (x0 y0) с нормалью (nx ny).

(defn intersect-time [[^double ax ^double vx ^double x]
                      [^double ay ^double vy ^double y]
                      [^double nx ^double ny ^double x0 ^double y0]]
  (let [a (* 0.5 (+ (* nx ax) (* ny ay)))
        b (+ (* nx vx) (* ny vy))
        c (+ (* nx (- x x0)) (* ny (- y y0)))]
    (solve-square-equation a b c)))
