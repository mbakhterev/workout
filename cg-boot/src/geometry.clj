(ns geometry)

(set! *warn-on-reflection* true)

; Структуры данных. Интуитивно могут быть полезны и повысить какую-нибудь
; эффективность

(defrecord Point [^double x ^double y])

(defrecord Roots [^double left ^double right])

; Прямая, заданная начальной точкой (x y) и нормалью (nx ny).

(defrecord Line [^double x ^double y ^double nx ^double ny])

; Отрезок поверхности. k и mx - это наклон и середина отрезка по оси x. Line
; задаёт линию, которую нельзя пересекать при полёте над секцией.

(defrecord Section [^double ax ^double ay
                    ^double bx ^double by
                    ^double k
                    ^double mx
                    ^Line   line])

(defrecord Landscape [^geometry.Section landing-pad
                      left-rock
                      right-rock
                      raw-surface])

(let [non-zero? (fn [x] (> x 1E-10))]
  (defn make-line [^Point a ^Point b]
    (let [x  (:x a)
          y  (:y a)
          dx (- (:x b) x)
          dy (- (:y b) y)
          ln (Math/sqrt (+ (* dx dx) (* dy dy)))]
      (assert (non-zero? ln))
      (->Line x y (- (/ dy ln)) (/ dx ln))))) 

(def ^:const ^:private uplift 32.0)

(defn- make-section [^Point a ^Point b]
  (let [l (make-line (assoc a :y (+ uplift (:y a)))
                     (assoc b :y (+ uplift (:y b))))]
    (->Section (:x a) (:y a)
               (:x b) (:y b)
               (double (/ (- (:y b) (:y a))
                          (- (:x b) (:x a))))
               (+ (:x a)
                  (/ (- (:x b) (:x a)) 2.0))
               l)))

(defn over-line? [^Line {x :x y :y nx :nx ny :ny} tx ty]
  (< 0 (+ (* nx (- tx x))
          (* ny (- ty y)))))

(defn- surface-points [raw-numbers]
  (map (fn [p] (apply ->Point p)) (partition 2 raw-numbers)))

(defn- surface-sections [points]
  (map (fn [s] (apply make-section s)) (partition 2 1 points)))

(defn- find-landing-pad [points]
  (letfn [(is-pad ([[a b]] (< -0.01 (- (:y a) (:y b)) 0.01)))]
    (apply make-section (first (filter is-pad (partition 2 1 points))))))

(defn- surface-shell [points landing]
  (letfn [(monotonize [points]
            (loop [[p & P] (rest points)
                   max-y (:y (first points))
                   R [(first points)]]
              (cond
                ; Обработка последней точки. Если она выше чем предыдущий
                ; максимальный уровень, то включаем её в список. Если нет, то
                ; накрываем поверхность горизонтальным сегментом на
                ; максимальном уровне
                (empty? P) (conj R (if (> (:y p) max-y) p (->Point (:x p) max-y)))
                
                ; Обновление максимума с добавлением точки в результат
                (> (:y p) max-y) (recur P (:y p) (conj R p))

                ; Пропускаем точку
                :else (recur P max-y R))))

          (sectionize [points]
            (map (fn [s] (apply make-section s)) (partition 2 1 points)))]
    (let [l-points (filter (fn [p] (<= (:x p) (:ax landing))) points)
          r-points (filter (fn [p] (>= (:x p) (:bx landing))) points)
          l-shell (reverse (monotonize (reverse l-points)))
          r-shell (monotonize r-points)]
      [(vec (sectionize l-shell)) (vec (sectionize r-shell))]))) 

(defn detect-landscape [surface-data]
  (let [points (surface-points surface-data)
        pad (find-landing-pad points)
        [l-rock r-rock] (surface-shell points pad)]
    (->Landscape pad l-rock r-rock (surface-sections points))))

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

; Мы решаем уравнение вида a*t*t + b*t + c относительно времени t.
; Из опыта решения Lander-2 можно сделать вывод, что нас всегда
; интересует один корень - время в ближайшем будущем. Его и вычисляем. 

(defn positive-root-of-square-equation [^double a ^double b ^double c]
  (or (if (zero? a)
        (if-not (zero? b)
          (let [t (/ (- c) b)]
            (if (<= 0.0 t) t)))
        (let [D (- (* b b) (* 4.0 a c))]
          (if (<= 0.0 D)
            (let [D-sqrt (Math/sqrt D)
                  a-rcpr (/ (* 2.0 a))
                  tp     (* (+ (- b) D-sqrt) a-rcpr)
                  tm     (* (- (- b) D-sqrt) a-rcpr)
                  t-max  (max tp tm)
                  t-min  (min tp tm)]
              (if (<= 0.0 t-min) t-min (if (<= 0.0 t-max) t-max))))))
      Double/NaN))

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

(defn intersect-time [^Line {x0 :x y0 :y nx :nx ny :ny}
                      [^double ax ^double vx ^double x]
                      [^double ay ^double vy ^double y]]
  (let [a (* 0.5 (+ (* nx ax) (* ny ay)))
        b (+ (* nx vx) (* ny vy))
        c (+ (* nx (- x x0)) (* ny (- y y0)))]
    (positive-root-of-square-equation a b c))) 
