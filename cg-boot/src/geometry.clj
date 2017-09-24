(ns geometry)

(set! *warn-on-reflection* true)

(def ^:const x-max (- 7000.0 1.0))
(def ^:const y-max (- 3000.0 1.0))

; Структуры данных. Интуитивно могут быть полезны и повысить какую-нибудь
; эффективность

(defrecord Point [^double x ^double y])

; Отрезок поверхности. Задаётся точками (point ax ay) и (point bx by),
; сразу содержит и нормаль (vector nx ny).

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
; цели x-target. Противоположный от цели конец сегмента x-opposite. Значения y-pad
; и x-pad задают высоту и дальний край посадочной площадки.
; Список сегментов surface - нужен для контроля пересечения при развороте на
; стадии reverse. Направление к цели задаётся direction так, чтобы 
; (<= 0.0 (* direction (- x-target (:x lander))))

(defrecord Stage [^Section section 
                  ^double x-target ^double x-opposite
                  ^double x-pad ^double y-pad
                  ^long direction 
                  stage
                  surface])

(let [ε 1E-10]
  (defn non-zero? [^double x] (< ε (Math/abs x)))
  (defn near-zero? [^double x] (<= (Math/abs x) ε)))

(defn- normal [^Point a ^Point b]
  (let [dx (- (:x b) (:x a))
        dy (- (:y b) (:y a))
        len (Math/sqrt (+ (* dx dx) (* dy dy)))]
    (assert (non-zero? len))
    [(- (/ dy len)) (/ dx len)]))

(defn- make-section [^Point a ^Point b]
  (let [[nx ny] (normal a b)] (->Section (:x a) (:y a)
                                         (:x b) (:y b)
                                         nx ny)))

(defn normal-projection ^double [^Section {x :ax y :ay nx :nx ny :ny}
                                 ^double tx ^double ty]
  (+ (* nx (- tx x)) (* ny (- ty y))))

(def over-line? (comp pos? normal-projection))
(defn in-range? [^double x ^Section {ax :ax bx :bx}] (and (<= ax x) (< x bx)))

(defn- surface-points [raw-numbers]
  (map (fn [p] (apply ->Point p)) (partition 2 raw-numbers)))

(defn- surface-sections [points]
  (map (fn [s] (apply make-section s)) (partition 2 1 points)))

(let [is-pad (fn [[^Point a ^Point b]] (near-zero? (- (:y a) (:y b))))]
  (defn- landing-pad [points]
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


(let [uplift-delta 64.0 ]
  (defn- uplift [^Section s] (->Section (:ax s) (+ uplift-delta (:ay s))
                                        (:bx s) (+ uplift-delta (:by s))
                                        (:nx s) (:ny s))))

(let [max-section-length 2048 ]
  (letfn [(x-split [^double a ^double b]
            (if (not (pos? (- b a max-section-length)))
              (list [a b])
              (let [m (+ a (/ (- b a) 2))] (concat (x-split a m) (x-split m b)))))]
    (defn- split-rock [^geometry.Section {ax :ax bx :bx ay :ay by :by nx :nx ny :ny}]
      (assert (> bx ax))
      (let [k (/ (- by ay) (- bx ax))
            remake (fn [[^double a ^double b]]
                     (let [δa (- a ax)
                           δb (- b ax)]
                       (->Section a (+ ay (* k δa))
                                  b (+ ay (* k δb))
                                  nx ny)))]
        (map remake (x-split ax bx))))))

(defn make-landscape ^Landscape [surface-data]
  (let [points (surface-points surface-data)
        pad (landing-pad points)
        [l-rock r-rock] (surface-shell points pad)]
    (->Landscape pad l-rock r-rock
                 (mapcat split-rock (map uplift l-rock))
                 (mapcat split-rock (map uplift r-rock))
                 (surface-sections points))))

(defn poly-2 ^double [^double a ^double b ^double c ^double x]
  (+ c (* x (+ b (* x a)))))

; Мы решаем уравнение (eq? 0 (poly 2 a b c x)) относительно времени.
; Из опыта решения Lander-2 можно сделать вывод, что нас всегда
; интересует один корень - время в ближайшем будущем. Его и вычисляем. Если
; решения нет, возвращается +бесконечность, потому что основной анализ на
; пересечения в том, что они будут позже определённого времени. С бесконечностью
; нужно проверять меньше условий.

(defn positive-root-of-square-equation ^double [^double a ^double b ^double c]
  (or (if (zero? a)
        (if-not (zero? b)
          (let [t (/ (- c) b)]
            (if (< 0.0 t) t)))
        (let [D (- (* b b) (* 4.0 a c))]
          (if (<= 0.0 D)
            (let [D-sqrt (Math/sqrt D)
                  a-rcpr (/ (* 2.0 a))
                  tp     (* (+ (- b) D-sqrt) a-rcpr)
                  tm     (* (- (- b) D-sqrt) a-rcpr)
                  t-max  (max tp tm)
                  t-min  (min tp tm)]
              (if (< 0.0 t-min) t-min (if (< 0.0 t-max) t-max))))))
      Double/POSITIVE_INFINITY))

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

; Время достижения сброса некоторой скорости до нуля. Как и прежде, +Inf
; означает «никогда»

(defn time-to-speed ^double [^double a ^double v ^double v-target]
  (let [t (/ (- v-target v) a)]
    (if (>= t 0.0) t Double/POSITIVE_INFINITY)))

(defn time-to-brake ^double [^double a ^double v] (time-to-speed a v 0.0))

; Построение стадий полёта.

(declare descend-stage
         brake-stage
         hover-stages
         reverse-stage)

; По странному ограничению в Clojure не может быть больше 4-ёх аргументов у
; функций, принимающих примитивные значения (WTF!? Вообще). Поэтому вычисляем
; то, над какой частью скал летим, внешним образом

(defn make-stages [^double x ^double vx
                   ^Landscape {l-rock :l-rock r-rock :r-rock pad :landing-pad}]
  (let [rock (if (< x (:ax pad)) l-rock r-rock)]
    (concat (reverse-stage x vx pad rock)
            (hover-stages x vx pad rock)
            (brake-stage x vx pad)
            (descend-stage x pad))))

(defn- brake-stage [^double x ^double vx
                    ^Section {ax :ax py :ay bx :bx :as pad}]
  (if-not (and (zero? vx) (in-range? pad x))
    (let [dir (if (or (< x ax) (< 0.0 vx)) 1 -1)
          px  (if (pos? dir) bx ax)
          ox  (if (pos? dir) ax bx)]
      (list (->Stage pad px ox px py dir :brake nil)))))

(defn- need-to-reverse? [^double x ^double vx ^Section {ax-pad :ax bx-pad :bx}]
  (or (and (< x ax-pad) (< vx 0.0))
      (and (> x bx-pad) (> vx 0.0))))

(defn- hover-stages [^double x ^double vx
                     ^Section {ax-pad :ax y-pad :ay bx-pad :bx :as pad}
                     rock]
  (let [going-ok? (not (need-to-reverse? x vx pad))]
    (letfn [(on-left [^Section s]
              (if (and (< x (:bx s) bx-pad)
                       (or going-ok? (< x (:ax s))))
                (->Stage s (:bx s) (:ax s) bx-pad y-pad 1 :hover nil)))
            (on-right [^Section s]
              (if (and (> x (:ax s) ax-pad)
                       (or going-ok? (> x (:bx s))))
                (->Stage s (:ax s) (:bx s) ax-pad y-pad -1 :hover nil)))]
      (cond
        (< x ax-pad) (keep on-left rock)
        (> x bx-pad) (keep on-right (reverse rock))))))

(defn- reverse-stage [^double x ^double vx
                      ^Section {ax-pad :ax y-pad :ay bx-pad :bx :as pad}
                      rock]
  (if (need-to-reverse? x vx pad) 
    (let [dir (if (< x ax-pad) 1 -1)
          s (first (filter (partial in-range? x) rock))
          surface (if (pos? dir)
                    (take-while (fn [^Section r] (<= (:ax r) x)) rock)
                    (drop-while (fn [^Section r] (<= (:bx r) x)) rock))]
      (list (->Stage s
                     (if (pos? dir) (:bx s) (:ax s)) (if (pos? dir) 0.0 x-max)
                     (if (pos? dir) bx-pad ax-pad) y-pad
                     dir
                     :reverse
                     (vec surface))))))

(defn- descend-stage [^double x
                      ^Section {ax-pad :ax y-pad :ay bx-pad :bx :as pad}]
  (let [dir (if (< x ax-pad) 1 -1)
        tx-pad (if (pos? dir) bx-pad ax-pad)
        ox-pad (if (pos? dir) ax-pad bx-pad)]
    (list (->Stage pad tx-pad ox-pad tx-pad y-pad dir :descend nil))))
