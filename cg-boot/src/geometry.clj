(ns geometry)

(set! *warn-on-reflection* true)

; Структуры данных. Интуитивно могут быть полезны и повысить какую-нибудь
; эффективность

(defrecord Point [^double x ^double y])

(defrecord Roots [^double left ^double right])

; Отрезок поверхности. Задаётся точками (point ax ay) и (point bx by),
; сразу содержит и нормаль (vector nx ny).

(comment (defrecord Section [^double ax ^double ay
                             ^double bx ^double by
                             ^double k
                             ^double mx]))

(defrecord Section [^double ax ^double ay
                    ^double bx ^double by
                    ^double nx ^double ny])

(defrecord Landscape [^geometry.Section landing-pad
                      left-rock
                      right-rock
                      raw-surface])

; Структура Stage для описания текущей стадии полёта. Содержит данные,
; необходимые для рассчётов. Границы ax и bx отрезка поверхности, проще
; проверить ax ≤ (:x lander) < bx, чем с учётом направления left? проверять
; пересечение с целью. Целевую линию x = tx необходимо пересечь. Дальняя граница
; посадочной площадки px и её высота py. Нормаль nx, ny и точка tx, ty задают
; линию, под которую нельзя залетать. Список surface -- набор дополнительных
; ограничительных отрезков для стадии reverse

(defrecord Stage [stage
                  surface
                  ^double ax ^double bx
                  ^double tx ^double ty
                  ^double nx ^double ny
                  ^double px ^double py
                  ^double direction
                  ^boolean left?])

(comment (defrecord Stage [stage
                           ^boolean left?
                           ^geometry.Section section 
                           ^geometry.Section pad
                           ^double x-goal
                           ^double x-pad
                           ^double y-pad
                           surface]))

(defn non-zero? [^double x] (< 1E-10 (Math/abs x)))

(defn- normal [^Point a ^Point b]
  (let [dx (- (:x b) (:x a))
        dy (- (:y b) (:y a))
        len (Math/sqrt (+ (* dx dx) (* dy dy)))]
    (assert (non-zero? len))
    [(- (/ dy len)) (/ dx len)]))

(comment (defn make-line [^Point a ^Point b]
           (let [x  (:x a)
                 y  (:y a)
                 dx (- (:x b) x)
                 dy (- (:y b) y)
                 ln (Math/sqrt (+ (* dx dx) (* dy dy)))]
             (assert (non-zero? ln))
             (->Line x y (- (/ dy ln)) (/ dx ln))))) 

(def ^:const ^:private uplift 32.0)

(comment (defn- make-section [^Point a ^Point b]
           (->Section (:x a) (:y a)
                      (:x b) (:y b)
                      (double (/ (- (:y b) (:y a))
                                 (- (:x b) (:x a))))
                      (+ (:x a)
                         (/ (- (:x b) (:x a)) 2.0))
                      (make-line (assoc a :y (+ uplift (:y a)))
                                 (assoc b :y (+ uplift (:y b)))))))


(defn- make-section [^Point a ^Point b]
  (let [[nx ny] (normal a b)] (->Section (:x a) (:y a)
                                         (:x b) (:y b)
                                         nx ny)))

(defn over-line? [^double x ^double y ^double nx ^double ny ^double tx ^double ty]
  (< 0 (+ (* nx (- tx x))
          (* ny (- ty y)))))

(defn in-range? [^double ax ^double bx ^double x] (and (<= ax x) (< x bx)))

(defn- surface-points [raw-numbers]
  (map (fn [p] (apply ->Point p)) (partition 2 raw-numbers)))

(defn- surface-sections [points]
  (map (fn [s] (apply make-section s)) (partition 2 1 points)))

(let [is-pad (fn [[^Point a ^Point b]] (not (non-zero? (- (:y a) (:y b)))))]
  (defn- find-landing-pad [points]
    (apply make-section (first (filter is-pad (partition 2 1 points))))))

(let [monotonize (fn [points]
                   (loop [[^Point p & P] (rest points)
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
                       :else (recur P max-y R))))]
  (defn- surface-shell [points landing]
    (let [l-points (filter (fn [^Point p] (<= (:x p) (:ax landing))) points)
          r-points (filter (fn [^Point p] (>= (:x p) (:bx landing))) points)
          l-shell (reverse (monotonize (reverse l-points)))
          r-shell (monotonize r-points)]
      [(vec (surface-sections l-shell)) (vec (surface-sections r-shell))])))

(defn build-landscape [surface-data]
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

; Мы решаем уравнение (poly 2 a b c) относительно времени.
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

; Построение стадий полёта.

(declare descend-stage
         brake-stage
         hover-stages
         reverse-stage)

(defn detect-stages [^double x ^double vx
                     ^Landscape {l-rock :left-rock r-rock :right-rock pad :landing-pad}]
  (->> (descend-stage x vx pad)
       (brake-stage x vx pad)
       (hover-stages x vx pad l-rock r-rock)
       (reverse-stage x vx pad l-rock r-rock)))

(comment (defn- brake-stage [^Lander {x :x vx :vx :as lander}
                             ^geometry.Section {ax :ax ay :ay bx :bx :as pad}
                             stages]
           (if (and (= 0.0 vx) (in-range? lander pad))
             stages
             (let [left? (or (< x ax) (< 0.0 vx))
                   xp (if left? bx ax)]
               (cons (->Stage :brake left? pad pad xp xp ay nil) stages)))))

(defn- brake-stage [^double x ^double vx
                    ^Section {ax :ax py :ay bx :bx nx :nx ny :ny :as pad}
                    stages]
  (if (and (= 0.0 vx) (in-range? ax bx x))
    stages
    (let [left? (or (< x ax) (< 0.0 vx))
          dir (if left? 1.0 -1.0) 
          px (if left? bx ax)]
      (cons (->Stage :brake nil ax bx px py nx ny px py dir left?) stages))))

(defn- hover-stages [^Lander {x :x :as l}
                     ^geometry.Section {ax :ax ay :ay bx :bx :as pad}
                     l-rock r-rock stages]
  (letfn [(on-left [^Stage s]
            (if (< x (:bx s) bx)
              (map (fn [^double t] (->Stage :hover true s pad t bx ay nil))
                   (divide-stage (max x (:ax s)) (:bx s)))))
          (on-right [^Stage s]
            (if (> x (:ax s) ax)
              (map (fn  [^double t] (->Stage :hover false s pad t ax ay nil))
                   (divide-stage  (min x (:bx s)) (:ax s)))))
          (divide-stage [^double a ^double t]
            (if (< (Math/abs (- t a)) 2048.0)
              (list t)
              (let [m (+ a (/ (- t a) 2.0))] (concat (divide-stage a m) (divide-stage m t)))))]
    (concat (cond (< x ax) (mapcat on-left l-rock)
                  (> x bx) (mapcat on-right (reverse r-rock)))
      stages)))

(defn- reverse-stage [^Lander {x :x vx :vx :as lander}
                      ^geometry.Section {ax :ax ay :ay bx :bx :as pad}
                      l-rock r-rock stages]
  (if-not (or (and (< x ax) (< vx 0.0))
              (and (> x bx) (> vx 0.0)))
    stages
    (let [left? (< x ax)
          rock (if left? l-rock r-rock)
          s (first (filter (partial in-range? lander) rock))
          surface (if left?
                    (take-while (fn [r] (<= (:ax r) x)) rock)
                    (drop-while (fn [r] (<= (:bx r) x)) rock))]
      (cons (->Stage :reverse left? s pad (if left? (:bx s) (:ax s)) (if left? bx ax) ay surface)
            stages))))

(defn- descend-stage [^Lander {x :x}
                      ^geometry.Section {ax :ax ay :ay bx :bx :as pad}]
  (let [left? (< x ax)
        xp (if left? bx ax)]
    (list (->Stage :descend left? pad pad xp xp ay nil))))



