
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

; Участок поверхности -- линия, проходящая через точки с заранее рассчитанной
; нормалью

(defrecord Section [^double ax ^double ay
                    ^double bx ^double by
                    ^double nx ^double ny])

; Местность для полёта. Списки сегментов l-rock и l-rock -- это приподнятые
; сегменты поверхности, чтобы не приходилось многократно пересчитывать этот
; подъём в самых неподходящих местах кода

(defrecord Landscape [^Section landing-pad
                      left-rock
                      right-rock
                      l-rock
                      r-rock
                      raw-surface])

; Структура Stage описывает стадию полёта над сегментом поверхности section к
; цели tx. Значения py и px задают высоту и дальний край посадочной площадки.
; Список сегментов surface - нужен для контроля пересечения при развороте на
; стадии reverse. Направление к цели задаётся direction так, чтобы 
; (<= 0.0 (* direction (- tx (:x lander))))

(defrecord Stage [^Section section 
                  ^double x-target
                  ^double x-pad ^double y-pad
                  ^long direction 
                  stage
                  surface])

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

(defn over-line? [^Section {x :ax y :ay nx :nx ny :ny} ^double tx ^double ty]
  (< 0 (+ (* nx (- tx x))
          (* ny (- ty y)))))

(defn in-range? [^double x ^Section {ax :ax bx :bx}] (and (<= ax x) (< x bx)))

(defn- surface-points [raw-numbers]
  (map (fn [p] (apply ->Point p)) (partition 2 raw-numbers)))

(defn- surface-sections [points]
  (map (fn [s] (apply make-section s)) (partition 2 1 points)))

(let [is-pad (fn [[^Point a ^Point b]] (not (non-zero? (- (:y a) (:y b)))))]
  (defn- find-landing-pad [points]
    (apply make-section (first (filter is-pad (partition 2 1 points))))))

(letfn [(monotonize [points]
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

(let [uplift-delta 32.0
      uplift (fn [^Section s] (->Section (:ax s) (+ uplift-delta (:ay s))
                                         (:bx s) (+ uplift-delta (:by s))
                                         (:nx s) (:ny s)))]
  (defn build-landscape [surface-data]
    (let [points (surface-points surface-data)
          pad (find-landing-pad points)
          [l-rock r-rock] (surface-shell points pad)]
      (->Landscape pad l-rock r-rock
                   (map uplift l-rock) (map uplift r-rock)
                   (surface-sections points)))))

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

(defn positive-root-of-square-equation ^double [^double a ^double b ^double c]
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

(defn time-to-intersect-2d ^double [[^double ax ^double vx ^double x]
                                    [^double ay ^double vy ^double y]
                                    ^Section {x0 :ax y0 :ay nx :nx ny :ny}]
  (let [a (* 0.5 (+ (* nx ax) (* ny ay)))
        b (+ (* nx vx) (* ny vy))
        c (+ (* nx (- x x0)) (* ny (- y y0)))]
    (positive-root-of-square-equation a b c))) 

; Время пересечения линии x = tx

(defn time-to-intersect-1d ^double [^double ax ^double vx ^double x ^double tx]
  (positive-root-of-square-equation (* 0.5 ax) vx (- x tx)))

; Построение стадий полёта.

(declare descend-stage
         brake-stage
         hover-stages
         reverse-stage)

(comment (defn detect-stages [^double x ^double vx
                              ^Landscape {l-rock :l-rock r-rock :r-rock pad :landing-pad}]
           (->> (descend-stage x vx pad)
                (brake-stage x vx pad)
                (hover-stages x pad l-rock r-rock)
                (reverse-stage x vx pad l-rock r-rock))))

; По странному ограничению в Clojure не может быть больше 4-ёх аргументов у
; функций, принимающих примитивные значения (WTF!? Вообще). Поэтому вычисляем
; то, над какой частью скал летим, внешним образом

(defn detect-stages [^double x ^double vx
                     ^Landscape {l-rock :l-rock r-rock :r-rock pad :landing-pad}]
  (let [rock (if (< x (:ax pad)) l-rock r-rock)]
    (concat (reverse-stage x vx pad rock)
            (hover-stages x pad rock)
            (brake-stage x vx pad)
            (descend-stage x vx pad))))

(comment (defn- brake-stage [^Lander {x :x vx :vx :as lander}
                             ^geometry.Section {ax :ax ay :ay bx :bx :as pad}
                             stages]
           (if (and (= 0.0 vx) (in-range? lander pad))
             stages
             (let [left? (or (< x ax) (< 0.0 vx))
                   xp (if left? bx ax)]
               (cons (->Stage :brake left? pad pad xp xp ay nil) stages)))))

(defn- brake-stage [^double x ^double vx
                    ^Section {ax :ax py :ay bx :bx nx :nx ny :ny :as pad}]
  (if-not (and (zero? vx) (in-range? pad x))
    (let [dir (if (or (< x ax) (< 0.0 vx)) 1 -1)
          px  (if (pos? dir) bx ax)]
      (list (->Stage pad px px py dir :brake nil)))))

(comment (defn- hover-stages [^double x
                              ^Section {ax :ax ay :ay bx :bx :as pad}
                              l-rock r-rock
                              stages]
           (letfn [(on-left [^Section s]
                     (if (< x (:bx s) bx)
                       (map (fn [^double t] (->Stage :hover true s pad t bx ay nil))
                            (divide-stage (max x (:ax s)) (:bx s)))))
                   (on-right [^Section s]
                     (if (> x (:ax s) ax)
                       (map (fn  [^double t] (->Stage :hover false s pad t ax ay nil))
                            (divide-stage  (min x (:bx s)) (:ax s)))))
                   (divide-stage [^double a ^double t]
                     (if (< (Math/abs (- t a)) 2048.0)
                       (list t)
                       (let [m (+ a (/ (- t a) 2.0))] (concat (divide-stage a m) (divide-stage m t)))))]
             (concat (cond (< x ax) (mapcat on-left l-rock)
                           (> x bx) (mapcat on-right (reverse r-rock)))
               stages))))

(defn- hover-stages [^double x
                     ^Section {ax-pad :ax y-pad :ay bx-pad :bx :as pad}
                     rock]
  (letfn [(on-left [^Section s]
            (if (< x (:bx s) bx-pad)
              (map (fn [^double t] (->Stage s t bx-pad y-pad 1 :hover nil))
                   (divide-stage (max (:ax s) x) (:bx s)))))
          (on-right [^Section s]
            (if (> x (:ax s) ax-pad)
              (map (fn [^double t] (->Stage s t ax-pad y-pad -1 :hover nil))
                   (divide-stage (:ax s) (min x (:bx s))))))
          (divide-stage [^double a ^double t]
            (if (< (- t a) 2048.0)
              (list t)
              (let [m (+ a (/ (- t a) 2.0))] (concat (divide-stage a m) (divide-stage m t)))))]
    (cond (< x ax-pad) (mapcat on-left rock)
          (> x bx-pad) (mapcat on-right (reverse rock)))))

(comment (defn- reverse-stage [^Lander {x :x vx :vx :as lander}
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
                     stages)))))

(defn- reverse-stage [^double x ^double vx
                      ^Section {ax-pad :ax y-pad :ay bx-pad :bx :as pad}
                      rock]
  (if (or (and (< x ax-pad) (< vx 0.0))
          (and (> x bx-pad) (> vx 0.0)))
    (let [dir (if (< x ax-pad) 1 -1)
          s (first (filter (partial in-range? x) rock))
          surface (if (pos? dir)
                    (take-while (fn [^Section r] (<= (:ax r) x)) rock)
                    (drop-while (fn [^Section r] (<= (:bx r) x)) rock))]
      (list (->Stage s (if (pos? dir) (:bx s) (:ax s)) (if (pos? dir) bx-pad ax-pad) y-pad dir :reverse surface)))))

(defn- descend-stage [^double x
                      ^Section {ax-pad :ax y-pad :ay bx-pad :bx :as pad}]
  (let [dir (if (< x ax-pad) 1 -1)
        tx-pad (if (pos? dir) bx-pad ax-pad)]
    (list (->Stage pad tx-pad tx-pad y-pad dir :descend nil))))
