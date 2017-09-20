(ns lander (:require [geometry :as g]))

(set! *warn-on-reflection* true)

(defn debugln [flag & args]
  (let [flags (hash-set ; :hover-search
                        ; :search-moves
                        ; :solve-hover
                        ; :brake-integrate
                        ; :solve-brake-4
                        ; :hover-search
                        ; :hover-integrate
                        ; :solve-descend-one
                        ; :along-guide
                        :make-guide
                        ; :model-guide
                        )]
    (if (flags flag) (binding [*out* *err*] (apply println args)))))

(defrecord Control [^long angle ^long power])

(defrecord Lander [^double x
                   ^double y
                   ^double vx
                   ^double vy
                   ^long fuel
                   ^Control control])

(defrecord Constraint [^double x ^double h ^double t])

(defn make-lander ^Lander [raw-numbers]
  (let [[m c] (split-at 5 raw-numbers)]
    (apply ->Lander (conj (vec m) (apply ->Control c)))))

(defrecord Move [state ^Lander lander ^double dt])

(let [angle-max-delta 15
      power-max-delta 1
      tune-value (fn [^long current ^long goal ^long max-delta]
                   (let [delta (- goal current)]
                     (cond (= 0 delta) goal
                           (< 0 delta) (if (< delta max-delta) goal (+ current max-delta))
                           (> 0 delta) (if (> delta (- max-delta)) goal (- current max-delta)))))]
  (defn- control-to ^Control [^Control f ^Control t]
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
    (^double [^Control {a :angle p :power}] (x-acceleration a p))
    (^double [^long a ^long p] ((x-table p) (+ 90 a))))
  (defn- y-acceleration
    (^double [^Control {a :angle p :power}] (y-acceleration a p))
    (^double [^long a ^long p] ((y-table p) (+ 90 a)))))

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

; Проверки положения модуля

(defn- in-range? [^Lander {x :x} ^geometry.Section s] (g/in-range? x s))
(defn- over-line? [^Lander {x :x y :y} ^geometry.Section s] (g/over-line? s x y))
(defn- over-section? [^Lander l ^geometry.Section s] (and (in-range? l s) (over-line? l s)))
(defn- on-radar? [^Lander {x :x y :y}] (and (<= 0 x g/x-max) (<= 0 y g/y-max)))

(defn alive? [surface ^Lander {x :x y :y :as l}]
  (and (on-radar? l)
       (over-line? l (first (filter (partial in-range? l) surface)))))

; Рассчёты для стадии последнего снижения: погашение вертикальной скорости с
; управлением (vec 0 4) 

(let [dx-reserve 0.125
      dh-reserve 0.125]
  (defn- reserve-dx ^double [^double x] (+ x (* x dx-reserve)))
  (defn- reserve-dh ^double [^double h] (+ h (* h dh-reserve))))

; Вычисляем разницу высот, на которой можем погасить vy, полагая, что управление
; уже (vec 0 4). Вычисления в обычной системе координат: Марс внизу. Если
; скорость такая, что гасить её не надо, нас это устраивает и мы отвечаем, что
; не нужна высота и время на сжигание топлива.

(def ^:const ^:private max-final-vy 38.0)
(def ^:const ^:private max-final-vx 20.0)

(defn- descend-constraint ^Constraint [^Lander {vy :vy}]
  (let [ay (y-acceleration 0 4)
        t  (g/time-to-speed ay vy (- max-final-vy))]
    (if (Double/isInfinite t)
      (->Constraint 0.0 0.0 0.0)
      (->Constraint 0.0 (g/poly-2 (* 0.5 ay) vy 0 t) t))))

(defn- brake-constraint ^Constraint [^Lander {vx :vx vy :vy} ^long direction]
  (let [φ  (* direction 90)
        ay (y-acceleration φ 4)
        ax (x-acceleration φ 4)
        t  (g/time-to-brake ax vx)]
    (if (<= 0.0 t)
      (->Constraint (+ (* vx t) (* 0.5 ax ax t)) 
                    (+ (* vy t) (* 0.5 ay ay t))
                    t))))

(defn- constraint [^Lander {x :x h :y fuel :fuel :as l}
                   ^geometry.Stage {xp :x-pad yp :y-pad dir :direction}]
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
         (or (g/non-zero? (g/normal-projection s x-next y-next)) (g/over-line? s x y))
         (< dt (g/time-to-intersect-1d ay vy y g/y-max))
         (or (g/non-zero? (- y g/y-max)) (<= y-next g/y-max))
         (< dt (g/time-to-intersect-1d ax vx x ox))
         (or (g/non-zero? (- x ox)) (pos? (* dir (- x-next ox)))))))

(defn- hover-align-control ^Move [^geometry.Stage {xt :x-target dir :direction :as stage}
                                  ^Lander l-init
                                  ^Control c-target]
  (loop [{c :control x :x :as l} l-init t 0.0]
    (cond (= c c-target) (->Move :ok l t)
          (pos? (* dir (- x xt))) (if (constraint l stage) (->Move :out l t) (->Move :ko l 0.0))
          :else (let [c-next (control-to c c-target)]
                  (if (hover-alive? stage l c-next 1.0)
                    (recur (just-move c-next 1.0 l) (+ 1.0 t))
                    (->Move :ko l 0.0))))))

; Логика точно такая же: сначала проверяем, можем ли достичь цели. Если можем,
; то за какое время. И не пересечём ли за это время: землю, небо и другую
; границу отрезка (такое теоретически может быть; возможно, это overkill).

(defn- hover-steady-control ^Move [^Stage {xt :x-target :as stage}
                                   ^Lander {x :x y :y vx :vx vy :vy c :control :as lander}]
  (let [ax (x-acceleration c)
        t  (Math/ceil (g/time-to-intersect-1d ax vx x xt))]
    (if (and (Double/isFinite t)
             (hover-alive? stage lander c t))
      (let [l (just-move c t lander)]
        (debugln :hover-search "hsc. constraint" (constraint l stage))
        (if (constraint l stage) (->Move :ok l t))))))

(defn- hover-integrate [^geometry.Stage stage ^Lander lander ^Control c]
  (let [{state :state :as m-align} (hover-align-control stage lander c)]
    (debugln :hover-search "hi. align state:" state)
    (case state
      :ko nil
      :ok (let [m-steady (hover-steady-control stage (:lander m-align))]
            (debugln :hover-search "hi. steady move:" m-steady)
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
      (debugln :hover-search (count moves-cloud))
      (when-first [m moves-cloud]
        (if-let [m-next (search-moves next-stages (:lander (first m)))]
          (conj m-next m)
          (recur (next moves-cloud)))))))

; Торможение - это 3 стадии: (1) торможение с переходом к выбранному контролю;
; (2) торможение с выбранным контролем; (3) торможение во время перехода к
; (контроль 0 4); Поэтому несколько движений. Решение на стадии (2) можно
; принимать лишь после моделирования стадии 3. Поэтому порядок таков. Кажется,
; движение аддитивно, поэтому можно обойтись без дополнительных повторных
; рассчётов.  Вроде как, считать не долго, поэтому ограничения на высоту
; проверяем в самом конце.

; Стадии 1 и 3 рассчитываются циклом выравнивания управления.

(defn- brake-initial-ok? [^Lander {y :y :as lander} ^geometry.Stage {section :section}]
  (and (<= y g/y-max) (over-section? lander section)))

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

(defn- solve-brake-2 ^Move [^Lander {vx :vx :as l}
                            ^Control c
                            ^geometry.Stage stage]
  (let [ax (x-acceleration c)]
    ; Если ускорение по x нулевое, то нет смысла тормозить с таким ускорением.
    ; Но полёт при этом нормальный, поэтому просто возвращаем l. С текущим
    ; для l контролем
    (if (zero? ax)
      (->Move :ok l 0.0)
      (let [t-brake (Math/ceil (g/time-to-brake ax vx))]
        (if (and (Double/isFinite t-brake)
                 (brake-alive? stage l c t-brake))
          (->Move :ok (just-move c t-brake l) t-brake))))))

(defn- brake-integrate [^geometry.Stage {y-pad :y-pad :as stage} 
                        ^Lander l
                        ^Control c]
  (when-let [m-1 (brake-align-control l stage c)]
    (when-let [m-3 (brake-align-control (:lander m-1) stage (->Control 0 4))]
      (when-let [m-2 (solve-brake-2 (:lander m-3) c stage)]
        (let [l  (:lander m-2)
              dc (descend-constraint l)
              hr (+ (:y l) (reserve-dh (:h dc)))]
          (if (and (< y-pad hr)
                   (< (* 4.0 (:t dc)) (:fuel l)))
            (list m-3 m-2 m-1)))))))

; По скорости можно определить какой диапазон ускорений следует рассматривать

(let [l-cloud (vec (for [p (range 1 5) a (range 0 91 angle-delta)] (->Control a p)))
      r-cloud (vec (for [p (range 1 5) a (range -90 1 angle-delta)] (->Control a p)))]
  (defn- brake-control-cloud [^Lander {vx :vx}]
    (if (>= vx 0.0) l-cloud r-cloud)))

(defn- brake-search [^geometry.Stage stage next-stages ^Lander l-init]
  (if (brake-initial-ok? l-init stage)
    (loop [moves-cloud (keep (partial brake-integrate stage l-init) (brake-control-cloud l-init))]
      (when-first [m moves-cloud]
        (let [k (:lander (first m))
              l (:lander (second m))]
          (if-let [m-next (search-moves next-stages (assoc l :control (:control k)))]
            (conj m-next m)
            (recur (next moves-cloud))))))))

; Решение для стадии (4) торможения. Тонкости. (4.1) ищем такой угол a для
; (контроль a 4), который позволит погасить остаточную скорость, оставаясь в
; границах pad.

; Решение для стадии (4) торможения. Пробуем гасить остаточную скорость с управлениями
; (контроль (одно-из 7 -7) 4). Собственно, вот и вся логика. Функция quite-slow?
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
          (if (< y-pad (+ (:y l) (:h dc)))
            (->Move :ok l t)))))))

(defn- descend-search [^geometry.Stage stage ^Lander lander]
  (if (brake-initial-ok? lander stage)
    (loop [l lander R (list)]
      (if-let [m (solve-descend l stage)]
        (case (:state m)
          :done (list (conj R m))
          :ok (recur (:lander m) (conj R m)))))))

(defn- reverse-moves [^geometry.Stage stage ^Lander lander] (list))

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
  (defn model-guide [moves ^Lander l]
    (let [traces (trace-moves (mapcat reverse moves) l)]
      (debugln :model-guide "trace count:" (count traces) (reduce + (map count moves)))
      (when-first [t traces]
        (debugln :model-guide "first trace:" t)
        (conj (next traces) (conj t l))))))

(defn search-guide [stages ^Lander l] (model-guide (search-moves stages l) l))

(defn- along-guide-cloud [^Lander {{angle :angle power :power} :control :as l}]
  (for [p (range (max 0 (- power 1)) (min (+ power 1 1) 5))
        a (range (max -90 (- angle 15)) (min (+ angle 15 1) 91))]
    (move (->Control a p) 1.0 l)))

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
    (let [ig (first (reduce-kv (fn [[^long k ^double M :as r] ^long i ^Lander g]
                                 (let [mi (diff-landers l g)]
                                   (if (<= M mi) r [i mi])))
                               [0 (diff-landers l (nth guide 0))]
                               guide))]
      (debugln :along-guide "ig:" ig)
      (when (> (- (count guide) 1) ig)
        (debugln :along-guide "next:" (nth guide (+ 1 ig)))
        (let [target (nth guide (+ 1 ig))
              cloud (along-guide-cloud l)
              [delta closest] (apply min-key first (map (juxt (partial diff-landers target) identity) cloud))]
          [delta (:control closest)])))))

(defn flatten-guide [guide] (vec (apply concat guide)))
