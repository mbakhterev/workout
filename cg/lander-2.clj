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

; Местность для полёта. Списки сегментов l-rock и l-rock -- это приподнятые и
; дополнительно нарезанные, в случае необходимости  сегменты поверхности, чтобы
; не приходилось многократно пересчитывать эти подъём и разбиение в самых неподходящих местах кода

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

(defn normal-projection [^Section {x :ax y :ay nx :nx ny :ny}
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

(defn time-to-intersect-2d [[^double ax ^double vx ^double x]
                            [^double ay ^double vy ^double y]
                            ^Section {x0 :ax y0 :ay nx :nx ny :ny}]
  (let [a (* 0.5 (+ (* nx ax) (* ny ay)))
        b (+ (* nx vx) (* ny vy))
        c (+ (* nx (- x x0)) (* ny (- y y0)))]
    (positive-root-of-square-equation a b c))) 

; Время пересечения линии x = tx

(defn time-to-intersect-1d [^double ax ^double vx ^double x ^double tx]
  (positive-root-of-square-equation (* 0.5 ax) vx (- x tx)))

; Время достижения сброса некоторой скорости до нуля. Как и прежде, +Inf
; означает «никогда»

(defn time-to-speed [^double a ^double v ^double v-target]
  (let [t (/ (- v-target v) a)]
    (if (>= t 0.0) t Double/POSITIVE_INFINITY)))

(defn time-to-brake [^double a ^double v] (time-to-speed a v 0.0))

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
  (if-not (and (zero? vx) (in-range? x pad))
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

(ns lander (:require [geometry :as g]))

(set! *warn-on-reflection* true)

(defn debugln [flag & args]
  (comment (let [flags (hash-set ; :hover-search
                                 ; :after
                                 ; :search-moves
                                 ; :solve-hover
                                 ; :brake-search
                                 ; :hover-search
                                 ; :hover-integrate
                                 ; :solve-descend-one
                                 ; :along-guide
                                 ; :make-guide
                                 ; :model-guide
                                 ; :reverse-search
                                 ; :constraint
                                 )]
             (if (flags flag) (binding [*out* *err*] (apply println args))))))

(defrecord Control [^long angle ^long power])

(defrecord Lander [^double x
                   ^double y
                   ^double vx
                   ^double vy
                   ^long fuel
                   ^Control control])

(defrecord Move [state ^Lander lander ^double dt])

(defrecord Constraint [^double x ^double h ^double t])

(defn make-lander ^lander.Lander [raw-numbers]
  (let [[m c] (split-at 5 raw-numbers)]
    (apply ->Lander (conj (vec m) (apply ->Control c)))))

(let [angle-max-delta 15
      power-max-delta 1
      tune-value (fn ^long [^long current ^long goal ^long max-delta]
                   (let [delta (- goal current)]
                     (cond (= 0 delta) goal
                           (< 0 delta) (if (< delta max-delta) goal (+ current max-delta))
                           (> 0 delta) (if (> delta (- max-delta)) goal (- current max-delta)))))]
  (defn- control-to [^Control f ^Control t]
    (->Control (tune-value (:angle f) (:angle t) angle-max-delta)
               (tune-value (:power f) (:power t) power-max-delta))))

; Таблица ускорений в зависимости от угла и мощности. В рассчёте учитываем, что
; угол задаётся от оси (+ PI/2)

(let [M 3.711
      cos (fn [a] (Math/cos (Math/toRadians (+ 90 a))))
      sin (fn [a] (Math/sin (Math/toRadians (+ 90 a))))
      x-force (fn ^double [a p] (* p (cos a)))
      y-force (fn ^double [a p] (- (* p (sin a)) M))
      to-zero (fn ^double [^double a] (if (g/non-zero? a) a 0.0))

      make-table (fn [f] (vec (for [p (range 0 5)]
                                (vec (for [a (range -90 91)]
                                       (to-zero (f a p)))))))

      x-table (make-table x-force)
      y-table (make-table y-force)]
  (defn- x-acceleration
    ([^Control {a :angle p :power}] (x-acceleration a p))
    ([^long a ^long p] ((x-table p) (+ 90 a))))
  (defn- y-acceleration
    ([^Control {a :angle p :power}] (y-acceleration a p))
    ([^long a ^long p] ((y-table p) (+ 90 a)))))

; Движение модуля l при управлении (vec angle power). Сохраняем новое положение
; модуля и то управление, которое привело его в это положение. Положение -
; вектор в фазовом пространстве (vec x y dx dy fuel)

(defn- just-move ^Lander [^Control {a :angle p :power :as c}
                          ^double t
                          ^Lander {x :x y :y vx :vx vy :vy fuel :fuel}]
  (let [ax (x-acceleration a p)
        ay (y-acceleration a p)]
    (->Lander (g/poly-2 (* 0.5 ax) vx x t)
              (g/poly-2 (* 0.5 ay) vy y t)
              (+ vx (* ax t))
              (+ vy (* ay t))
              (- fuel (* p t))
              c)))

(defn move ^Lander [^Control c-target ^double t ^Lander {c :control :as l}]
  (assert (or (= 1.0 t) (= c c-target)))
  (let [c-next (control-to c c-target)] (just-move c-next t l)))

; Проверки того, что модуль над поверхностью Марса

; Проверки положения модуля

(defn- in-range? [^Lander {x :x} ^geometry.Section s] (g/in-range? x s))
(defn- over-line? [^Lander {x :x y :y} ^geometry.Section s] (g/over-line? s x y))
(defn- over-section? [^Lander l ^geometry.Section s] (and (in-range? l s) (over-line? l s)))
(defn- on-radar? [^Lander {x :x y :y}] (and (<= 0 x g/x-max) (<= 0 y g/y-max)))

(defn alive? [surface ^Lander {x :x y :y :as l}]
  (and (on-radar? l)
       (over-line? l (first (filter (partial in-range? l) surface)))))

; Рассчёты ограничений на движение модуля. Необходимо иметь запас пространства
; для горизонтального торможения с (контроль ±90 4) и посадки с (контроль 0 4).
; Код вычисления этих ограничений и дополнительного резервирования

(let [dx-reserve 0.125
      dh-reserve 0.125]
  (defn- reserve-dx ^double [^double x] (+ x (* x dx-reserve)))
  (defn- reserve-dh ^double [^double h] (+ h (* h dh-reserve))))

; Вычисляем разницу высот, на которой можем погасить vy, полагая, что управление
; уже (контроль 0 4). Вычисления в обычной системе координат: Марс внизу. Если
; скорость такая, что гасить её не надо, нас это устраивает и мы отвечаем, что
; не нужна высота и время на сжигание топлива.

(def ^:const ^:private max-final-vy 38.0)
(def ^:const ^:private max-final-vx 18.0)

(defn- descend-constraint ^Constraint [^Lander {vy :vy}]
  (let [ay (y-acceleration 0 4)
        t  (g/time-to-speed ay vy (- max-final-vy))]
    (if (Double/isInfinite t)
      (->Constraint 0.0 0.0 0.0)
      (->Constraint 0.0 (g/poly-2 (* 0.5 ay) vy 0.0 t) t))))

(defn- brake-constraint ^Constraint [^Lander {vx :vx vy :vy} ^long direction]
  (let [φ  (* direction 90)
        ay (y-acceleration φ 4)
        ax (x-acceleration φ 4)
        t  (g/time-to-brake ax vx)]
    (if (Double/isInfinite t)
      (->Constraint 0.0 0.0 0.0)
      (->Constraint (+ (* vx t) (* 0.5 ax ax t)) 
                    (+ (* vy t) (* 0.5 ay ay t))
                    t))))

(defn- constraint [^geometry.Stage {xp :x-pad yp :y-pad dir :direction}
                   ^Lander {x :x h :y fuel :fuel :as l}]
  (if-let [bc (brake-constraint l dir)]
    (let [dc (descend-constraint l)
          xr (+ x (reserve-dx (:x bc)))
          hr (+ h (reserve-dh (+ (:h bc) (:h dc))))]
      (and (neg? (* dir (- xr xp)))
           (< yp hr)
           (< (* 4.0 (+ (:t bc) (:t dc))) fuel)))))

(declare hover-search
         brake-search
         descend-search
         reverse-search)

; Результатом search-moves должен быть список из списков шагов (Move) для каждой
; стадии. Списки шагов идут в обратном порядке, чтобы как можно быстрее
; вытаскивать последнее положение модуля на стадии (оно будет первым в списке и,
; вполне вероятно, быстро доступным). За это упорядочение отвечают нечтно-guide
; функции, которые передают эту обязанность в нечто-integrate функции.

(defn- search-moves [stages ^Lander l]
  (if-let [s (first stages)]
    (case (:stage s)
      :reverse (do (debugln :search-moves "reverse") (reverse-search s (rest stages) l))  
      :brake   (do (debugln :search-moves "brake") (brake-search s (rest stages) l))
      :hover   (do (debugln :search-moves "hover") (hover-search s (rest stages) l))
      :descend (do (debugln :search-moves "brake") (descend-search s l))
      :else    (assert false))))

; hover-align-control -- цикл выравнивания управления к заданному. Логика работы
; такая: сначала проверка того, что модуль не разобьётся при очередном шаге
; управления, а потом осуществление этого шага. Если всё хорошо, то модуль может
; либо достигнуть нужного контроля, либо вылететь за цель в процессе. В
; случае вылета за цель, стадию можно считать пройденной. Но тогда нужно
; проверять результат на то, что он укладывается в ограничения constraint. В
; случае неуспеха функция возвращает (move :ko l 0.0), это требуется для
; функции генерации облака управлений для стадии hover. Предполагаем, что
; исходный lander корректный для пролёта над секцией.

(defn- hover-initial-ok? [^Lander {y :y :as l} ^geometry.Stage {s :section}]
  (and (<= y g/y-max) (over-section? l s)))

; Проверка на допустимость движения под заданным управлением. Критерий: модуль
; не должен пересечь ни одну из ограничивающих стадию линий. Тонкость в том, что
; модуль может быть на этой линии. Тогда time-to-intersect (которая сводится к
; positive-root-of-square-equation, которая возвращает только строго
; положительные корни) пройдёт тест на пересечение (positive-root вернёт +Infinity или
; следующее пересечение с линией в достаточно отдалённом будущем), но при этом,
; сам модуль во время такого движения может оказаться не с той стороны от линии.
; Проверяем это

(defn- hover-alive? [^geometry.Stage {s :section dir :direction ox :x-opposite}
                     ^Lander {x :x y :y vx :vx vy :vy}
                     ^Control {a :angle p :power}
                     ^double dt]
  (let [ax (x-acceleration a p)
        ay (y-acceleration a p)
        x-next (g/poly-2 (* 0.5 ax) vx x dt)
        y-next (g/poly-2 (* 0.5 ay) vy y dt)]
    (and (< dt (g/time-to-intersect-2d [ax vx x] [ay vy y] s))
         (or (g/non-zero? (g/normal-projection s x y)) (g/over-line? s x-next y-next))
         (< dt (g/time-to-intersect-1d ay vy y g/y-max))
         (or (g/non-zero? (- y g/y-max)) (<= y-next g/y-max))
         (< dt (g/time-to-intersect-1d ax vx x ox))
         (or (g/non-zero? (- x ox)) (pos? (* dir (- x-next ox)))))))

(defn- hover-align-control ^Move [^geometry.Stage {xt :x-target dir :direction :as stage}
                                  ^Lander l-init
                                  ^Control c-target]
  (loop [{c :control x :x :as l} l-init t 0.0]
    (cond (= c c-target) (->Move :ok l t)
          (pos? (* dir (- x xt))) (if (constraint stage l) (->Move :out l t) (->Move :ko l 0.0))
          :else (let [c-next (control-to c c-target)]
                  (if (hover-alive? stage l c-next 1.0)
                    (recur (just-move c-next 1.0 l) (+ 1.0 t))
                    (->Move :ko l 0.0))))))

; Логика точно такая же: сначала проверяем, можем ли достичь цели. Если можем,
; то за какое время. И не пересечём ли за это время: землю, небо и другую
; границу отрезка (такое теоретически может быть; возможно, это overkill).

(defn- hover-steady-control ^Move [^Stage {xt :x-target :as stage}
                                   ^Lander {x :x vx :vx c :control :as lander}]
  (let [ax (x-acceleration c)
        t  (Math/ceil (g/time-to-intersect-1d ax vx x xt))]
    (if (and (Double/isFinite t)
             (hover-alive? stage lander c t))
      (let [l (just-move c t lander)]
        (if (constraint stage l) (->Move :ok l t))))))

(defn- hover-integrate [^geometry.Stage stage ^Lander lander ^Control c]
  (let [{state :state :as m-align} (hover-align-control stage lander c)]
    (case state
      :ko nil
      :ok (let [m-steady (hover-steady-control stage (:lander m-align))]
            (if m-steady (list m-steady m-align)))
      :out (list m-align))))

; Облако возможных управлений на стадии hover

(def ^:const ^:private angle-delta 15) 

(defn- hover-control-cloud [^Stage {dir :direction :as stage} ^Lander l]
  (for [p (range 0 5)
        a (let [a-left (:angle (:control (:lander (hover-align-control stage l (->Control -90 p)))))
                a-right (:angle (:control (:lander (hover-align-control stage l (->Control 90 p)))))]
            (if-not (pos? dir)
              (range a-left (+ a-right 1) angle-delta)
              (range a-right (- a-left 1) (- angle-delta))))]
  (->Control a p)))

(defn- hover-search [^geometry.Stage stage next-stages ^Lander l]
  (if (hover-initial-ok? l stage)
    (loop [moves-cloud (keep (partial hover-integrate stage l) (hover-control-cloud stage l))]
      (when-first [m moves-cloud]
        (if-let [m-next (search-moves next-stages (:lander (first m)))]
          (conj m-next m)
          (recur (next moves-cloud)))))))

(defn- brake-align-control [^Lander lander ^Stage {pad :pad} ^Control ctl]
  (loop [l lander t 0.0]
    (cond (not (on-radar? l))         nil
          (not (over-section? l pad)) nil
          (= ctl (:control l))        (->Move :ok l t)
          :else                       (recur (move ctl 1.0 l) (+ 1.0 t)))))

; Торможение - это 3 стадии: (1) торможение с переходом к выбранному контролю;
; (2) торможение с выбранным контролем; (3) торможение во время перехода к
; (контроль 0 4); Поэтому несколько движений. Решение на стадии (2) можно
; принимать лишь после моделирования стадии 3. Поэтому порядок таков. Кажется,
; движение аддитивно, поэтому можно обойтись без дополнительных повторных
; рассчётов.  Вроде как, считать не долго, поэтому ограничения на высоту
; проверяем в самом конце.

; Стадии 1 и 3 рассчитываются циклом выравнивания управления.

(defn- brake-initial-ok? [^Lander {y :y :as lander} ^geometry.Stage {pad :section}]
  (and (<= y g/y-max) (over-section? lander pad)))

(defn- brake-alive? [^geometry.Stage {{ax-pad :ax y-pad :ay bx-pad :bx} :section}
                     ^Lander {x :x y :y vx :vx vy :vy}
                     ^Control {a :angle p :power}
                     ^double dt]
  (let [ax (x-acceleration a p)
        ay (y-acceleration a p)
        x-next (g/poly-2 (* 0.5 ax) vx x dt)
        y-next (g/poly-2 (* 0.5 ay) vy y dt)]
    (and (< dt (g/time-to-intersect-1d ax vx x ax-pad))
         (or (g/non-zero? (- x ax-pad)) (>= x-next ax-pad))
         (< dt (g/time-to-intersect-1d ax vx x bx-pad))
         (or (g/non-zero? (- x bx-pad)) (<= x-next bx-pad))
         (< dt (g/time-to-intersect-1d ay vy y g/y-max))
         (or (g/non-zero? (- y g/y-max)) (<= y-next g/y-max))
         (< dt (g/time-to-intersect-1d ay vy y y-pad))
         (or (g/non-zero? (- y y-pad)) (<= y-pad y)))))

(defn- brake-align-control ^Move [^Lander l-init ^geometry.Stage stage ^Control c-target]
  (loop [{c :control :as l} l-init t 0.0]
    (if (= c c-target)
      (->Move :ok l t)
      (let [c-next (control-to c c-target)]
        (if (brake-alive? stage l c-next 1.0)
          (recur (just-move c-next 1.0 l) (+ 1.0 t)))))))

; Решение для стадии (2) торможения. Тонкости. (2.1) считаем, что целевая vx
; равна 0. (2.2) считаем, что должны хотя бы 0 секунд тормозить. Иначе, нам дали
; плохой контроль, и можно было бы потратить меньше топлива на остановку.

(defn- x-speed-adjust ^double [^Control c-from ^Control c-to ^double vx]
  (loop [v vx c c-from]
    (if (= c c-to)
      v
      (let [c-next (control-to c c-to)]
        (recur (+ v (x-acceleration c-next)) c-next)))))

(defn- speed-to-04 ^double [^Control c-from ^double vx] (x-speed-adjust c-from (->Control 0 4) vx))

(defn- solve-brake-2 ^Move [^Lander {vx :vx c :control :as l}
                            ^geometry.Stage {:as stage}]
  (let [ax (x-acceleration c)
        v-drop (speed-to-04 c vx)]
    ; Если ускорение по x нулевое, то нет смысла тормозить с таким ускорением.
    ; Если остаточная скорость v-drop меньше нуля вдоль направления торможения, то
    ; тормозить тоже нет смысла. Считаем, что в этих случаях полёт нормальный,
    ; возвращаем l с текущим контролем
    (if (or (zero? ax)
            (neg? (* vx v-drop)))
      (->Move :ok l 0.0)
      (let [t-brake (Math/ceil (g/time-to-brake ax v-drop))]
        (if (and (Double/isFinite t-brake)
                 (brake-alive? stage l c t-brake))
          (->Move :ok (just-move c t-brake l) t-brake))))))

(defn- brake-integrate [^geometry.Stage {y-pad :y-pad :as stage} 
                        ^Lander l
                        ^Control c]
  (when-let [m-1 (brake-align-control l stage c)]
    (when-let [m-2 (solve-brake-2 (:lander m-1) stage)]
      (when-let [m-3 (brake-align-control (:lander m-2) stage (->Control 0 4))]
        (let [l  (:lander m-3)
              dc (descend-constraint l)
              hr (+ (:y l) (reserve-dh (:h dc)))]
          (when (and (< y-pad hr)
                     (< (* 4.0 (:t dc)) (:fuel l)))
            (list m-3 m-2 m-1)))))))

; По скорости можно определить какой диапазон ускорений следует рассматривать

(let [l-cloud (vec (for [p (range 3 5) a (range 0 91 5)] (->Control a p)))
      r-cloud (vec (for [p (range 3 5) a (range -90 1 5)] (->Control a p)))]
  (defn- brake-control-cloud [^Lander {vx :vx}]
    (if (>= vx 0.0) l-cloud r-cloud)))

(defn- brake-search [^geometry.Stage stage next-stages ^Lander l-init]
  (if (brake-initial-ok? l-init stage)
    (loop [moves-cloud (keep (partial brake-integrate stage l-init) (brake-control-cloud l-init))]
      (when-first [m moves-cloud]
        (if-let [m-next (search-moves next-stages (:lander (first m)))]
          (conj m-next m)
          (recur (next moves-cloud)))))))

; Решение для стадии (4) торможения. Пробуем гасить остаточную скорость с управлениями
; (контроль ±7 4). Собственно, вот и вся логика. Функция quite-slow?
; определяет, достаточно ли медленно горизонтальное движение. Достаточность
; определяется как то, что время полёта до границы посадочного стакана меньше,
; чем время полёта до дна. Время полёта до дна - галимая эвристика: примерно
; считаем скоростью спуска -20.0. Код старается преувеличить это время спуска

(let [vy-average -20.0]
  (defn- quite-slow? [^Lander {x :x vx :vx y :y :as l}
                      ^geometry.Stage {pad :section}]
    (or (g/near-zero? vx)
        (let [x-pad (if (pos? vx) (:bx pad) (:ax pad))
              t-out (/ (- x-pad x) vx)
              t-drop (Math/ceil (/ (- (:ay pad) y) vy-average))]
          (and (< (Math/abs ^double vx) max-final-vx)
               (<= t-drop t-out))))))

(defn- solve-descend ^Move [^Lander {vx :vx :as l-init}
                            ^geometry.Stage {y-pad :y-pad :as stage}]
  (if (quite-slow? l-init stage)
    (->Move :done l-init 0.0)
    (let [φ  (if (neg? vx) -7 7)
          ax (x-acceleration φ 4)
          t  (Math/ceil (/ (- vx) ax))
          c  (->Control φ 4)]
      (if (brake-alive? stage l-init c t)
        (let [l  (just-move c t l-init)
              dc (descend-constraint l)]
          (if (and (< y-pad (+ (:y l) (:h dc)))
                   (< (* 4.0 (:t dc)) (:fuel l)))
            (->Move :ok l t)))))))

(defn- descend-search [^geometry.Stage stage ^Lander lander]
  (if (brake-initial-ok? lander stage)
    (loop [l lander R (list)]
      (if-let [m (solve-descend l stage)]
        (case (:state m)
          :done (list (conj R m))
          :ok (recur (:lander m) (conj R m)))))))

; Рассчёт управления для стадии reverse

(def ^:const ^:parivate reverse-initial-ok? hover-initial-ok?)

(defn- check-section [^double dt
                      [^double x-next ^double y-next]
                      [^double ax ^double vx ^double x :as x-params]
                      y-params]
  (fn [^geometry.Section s]
    (let [y (y-params 2)
          t-intersect (g/time-to-intersect-2d x-params y-params s)
          x-intersect (g/poly-2 (* 0.5 ax) vx x t-intersect)]
      (and (or (< dt t-intersect) (not (g/in-range? x-intersect s)))
           (or (g/non-zero? (g/normal-projection s x y)) (g/over-line? s x-next y-next))))))

(defn- reverse-alive? [^geometry.Stage {S :surface dir :direction xo :x-opposite}
                       ^Lander {x :x y :y vx :vx vy :vy}
                       ^Control {a :angle p :power}
                       ^double dt]
  (let [ax (x-acceleration a p)
        ay (y-acceleration a p)
        x-next (g/poly-2 (* 0.5 ax) vx x dt)
        y-next (g/poly-2 (* 0.5 ay) vy y dt)]
    (and (< dt (g/time-to-intersect-1d ay vy y g/y-max))
         (or (g/non-zero? (- y g/y-max)) (<= y-next g/y-max))
         (< dt (g/time-to-intersect-1d ax vx x xo))
         (or (g/non-zero? (- x xo)) (pos? (* dir (- x-next xo))))
         (every? (check-section dt [x-next y-next] [ax vx x] [ay vy y]) S))))

(defn- reverse-align-control ^Move [^geometry.Stage {xt :x-target dir :direction :as stage}
                                    ^Control C  
                                    ^Lander L]
  (loop [{c :control x :x :as l} L t 0.0]
    (cond (= c C) (->Move :ok l t)
          (pos? (* dir (- x xt))) (->Move :out l t) 
          :else (let [c-next (control-to c C)]
                  (if (reverse-alive? stage l c-next 1.0)
                    (recur (just-move c-next 1.0 l) (+ 1.0 t))
                    (->Move :ko l 0.0))))))

(defn- reverse-steady-control ^Move [^geometry.Stage {xt :x-target :as stage}
                                     ^Lander {x :x vx :vx C :control :as L}]
  (let [ax (x-acceleration C)
        t  (Math/ceil (g/time-to-intersect-1d ax vx x xt))]
    (if (and (Double/isFinite t)
             (reverse-alive? stage L C t))
      (->Move :ok (just-move C t L) t))))

; Логика примерно та же самая, что и в solve-brake-2. Отличие в том, что
; скорость меняется не до нуля, а до противоположной и равной speed по модулю

(defn- reverse-solve-brake ^Move [^geometry.Stage {dir :direction :as stage}
                                  ^double speed
                                  ^Control C
                                  ^Lander {vx :vx c :control :as l}]
  (let [ax (x-acceleration c)
        v-drop (x-speed-adjust c C vx)]
    (if (or (zero? ax)
            (neg? (* vx v-drop)))
      (->Move :ok l 0.0)
      (let [t-brake (Math/ceil (g/time-to-speed ax v-drop (* dir speed)))]
        (if (and (Double/isFinite t-brake)
                 (reverse-alive? stage l c t-brake))
          (->Move :ok (just-move c t-brake l) t-brake))))))

; Слишком много будет промежуточного case-анализа. Поэтому небольшая
; импровизация в monad-стиле

(defn- m-after [constraints-ok? next-move continue-with]
  (fn [^Lander L moves]
    (when-let [{st :state l :lander :as m} (next-move L)]
      (case st
        :ko nil
        :out (let [c (constraints-ok? l)]
               (if c (conj moves m)))
        :ok (let [c (constraints-ok? l)]
              (if c (continue-with l (conj moves m))))))))

(defn- m-extract [^Lander l moves] moves)

(defn- reverse-integrate [^geometry.Stage {dir :direction :as stage}
                          ^Lander L
                          [^Control c-brake speed ^Control c-target :as C]]
  (let [after (partial m-after (partial constraint stage))]
    ((after
       (partial reverse-align-control stage c-brake)
       (after
         (partial reverse-solve-brake stage speed c-target)
         (after
           (partial reverse-align-control stage c-target)
           (after
             (partial reverse-steady-control stage)
             m-extract))))
     L (list))))

; Облако управлений состоит из троек
; (tuple контроль-торможения
;        скорость-после-торможения
;        контроль-до-цели)

(let [make-cloud (fn [^long dir]
                   (let [δa (* dir (- angle-delta))
                         a-from 0
                         a-to (* dir -91)
                         P (range 4 5)
                         A (range a-from a-to δa)
                         C (for [p P a A] (->Control a p))
                         CT (for [p P a (range -90 91 angle-delta)] (->Control a p))
                         S (range 96 15 -16)]
                     (vec (for [c-brake (reverse C) speed S c-target CT] [c-brake speed c-target]))))
      l-cloud (make-cloud 1)
      r-cloud (make-cloud -1)]
  (defn- reverse-control-cloud [^geometry.Stage {dir :direction}]
    (if (pos? dir) l-cloud r-cloud)))

(defn- reverse-search [^geometry.Stage stage next-stages ^Lander l]
  (if (reverse-initial-ok? l stage)
    (let [rcc (reverse-control-cloud stage)]
      (loop [moves-cloud (doall (keep (partial reverse-integrate stage l) rcc))]
        (when-first [m moves-cloud]
          (if-let [m-next (search-moves next-stages (:lander (first m)))]
            (conj m-next m)
            (recur (next moves-cloud))))))))

; На каждый Move получаем список из Lander-ов, которые моделируют траекторию и
; управление с шагом в 1 секунду. Все Move сгруппированы по стадиям в списки.
; do-control сначала их собирает в плоский список, а потом каждый Move
; превращает в список Lander-моделей с шагом в 1.0 по времени

(letfn [(trace-moves [moves ^Lander l]
          (debugln :model-guide moves)
          (when-first [{{c :control} :lander t :dt} moves]
            (if (zero? t)
              (trace-moves (next moves) l)
              (let [trace (take t (next (iterate (partial move c 1.0) l)))]
                (conj (trace-moves (next moves) (last trace))
                      trace)))))]
  (defn- model-guide [moves ^Lander l]
    (let [traces (trace-moves (mapcat reverse moves) l)]
      (debugln :model-guide "trace count:" (count traces) (reduce + (map count moves)))
      (when-first [t traces]
        (debugln :model-guide "first trace:" t)
        (conj (next traces) (conj t l))))))

(defn search-guide [stages ^Lander l] (model-guide (search-moves stages l) l))

(defn- diff-landers [^Lander a ^Lander b]
  (let [dx (- (:x a) (:x b))
        dy (- (:y a) (:y b))
        dvx (- (:vx a) (:vx b))
        dvy (- (:vy a) (:vy b))]
    (+ (* dx dx)
       (* dy dy)
       (* dvx dvx)
       (* dvy dvy)))) 

(defn along-guide [^Lander l guide]
  (if (not (empty? guide))
    (let [[ig δ] (reduce-kv (fn [[^long k ^double M :as r] ^long i ^Lander g]
                              (let [mi (diff-landers l g)]
                                (if (<= M mi) r [i mi])))
                            [0 (diff-landers l (nth guide 0))]
                            guide)]
      (when (> (- (count guide) 1) ig)
        (let [target (nth guide (+ 1 ig))]
          [δ (:control target)])))))

(defn flatten-guide [guide] (vec (apply concat guide)))

(ns Player (:gen-class) (:require [lander :as l] [geometry :as g]))

(set! *warn-on-reflection* true)

(defn- dump [& args] (binding [*out* *err*] (apply println args)))
(defn- read-surface [] (let [N (read)] (doall (repeatedly (* 2 N) read))))
(defn- read-lander [] (doall (repeatedly 7 read)))

(defn- make-guide [^lander.Lander {x :x vx :vx :as l}
                   ^geometry.Landscape scape]
  (let [stages (g/make-stages x vx scape)]
    (l/debugln :make-guide stages)
    (let [guide (l/search-guide stages l)]
      (l/flatten-guide guide))))

(defn- trace-move [^lander.Control {a :angle p :power}] (println a p))
(defn- approximate-last ^lander.Lander [] (l/make-lander (read-lander)))

(defn- routine ^lander.Lander [^lander.Lander l ^lander.Control control guide]
  (when-let [[δ c] (if guide (l/along-guide l guide) [0.0 control])]
    (trace-move c)
    (approximate-last)))

(defn- done-it [what ^lander.Lander l]
  (dump (str what " is DONE.") "Last lander:" l)
  nil)

(def ^:private ^:const quanta 128)

(comment (defn- wait-loop [^geometry.Landscape scape
                           ^lander.Control control
                           ^lander.Lander lander]
           (loop [l lander G (future (make-guide lander scape)) steps 0]
             (if-let [g (deref G quanta nil)] 
               (do (dump "guide computation is DONE." "steps:" steps "guide length:" (count g))
                   (if (empty? g)
                     (do (dump "guide computation is FAILED. Continuing wait routine")
                         (trace-move control)
                         (let [tl (approximate-last)]
                           (recur tl (future (make-guide tl scape)) 0)))
                     [l g]))
               (do (dump "waiting for guide with control:" control)
                   (trace-move control)
                   (recur (approximate-last) G (+ 1 steps)))))))

(defn- wait-loop [^geometry.Landscape scape
                  ^lander.Control control
                  ^lander.Lander lander
                  g-known]
  (loop [l lander G (future (make-guide lander scape)) steps 0]
    (if-let [g (deref G quanta nil)] 
      (do (dump "guide computation is DONE." "steps:" steps "guide length:" (count g))
          (if (empty? g)
            (do (dump "guide computation is FAILED. Continuing with routine")
                (if-let [l-next (routine l control g-known)]
                  (recur l-next (future (make-guide l-next scape)) 0)
                  (done-it "routine" l)))
            [l g]))
      (do (dump "waiting for guide with routine")
          (if-let [l-next (routine l control g-known)]
            (recur l-next G (+ 1 steps))
            (done-it "routine" l))))))

(def ^:private ^:const tolerable-drift (* 4.0 4.0))

(comment (defn- guide-loop [^lander.Lander lander guide]
           (loop [l lander steps 0]
             (if-let [[delta control] (along-guide l guide)]
               (if (> delta tolerable-drift)
                 (do (dump "guide drift is too large. Correction is needed."
                           "delta:" delta
                           "steps:" steps
                           "x:" (:x l))
                     l)
                 (do (dump "delta is ok:" delta)
                     (trace-move control)
                     (recur (approximate-last) (+ 1 steps))))
               (do (dump "guide following is DONE")
                   nil)))))

(defn- guide-loop [^lander.Lander lander guide]
  (loop [l lander steps 0]
    (if-let [[delta control] (l/along-guide l guide)]
      (if (> delta tolerable-drift)
        (do (dump "guide drift is too large. Correction is needed."
                  "delta:" delta
                  "steps:" steps
                  "x:" (:x l))
            l)
        (do (dump "delta is ok:" delta)
            (trace-move control)
            (recur (approximate-last) (+ 1 steps))))
      (done-it "guide following" l))))

(defn -main [& args]
  (let [raw-surface (read-surface)
        raw-lander (read-lander)
        S (g/make-landscape raw-surface)
        L (l/make-lander raw-lander)]
    (dump "surface:" raw-surface)
    (dump "lander:" raw-lander)
    (loop [l L g-known nil]
      (let [[lw g] (wait-loop S (l/->Control 0 4) l g-known)]
        (dump "guide length:" (count g))
        (if-let [lg (guide-loop lw g)]
          (recur lg g)))))
  (while true (println 0 4) (read-lander)))
