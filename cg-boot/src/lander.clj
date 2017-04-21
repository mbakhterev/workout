(ns lander (:require [geometry :refer :all]))

(set! *warn-on-reflection* true)

(comment
  (defn- debugln [flag & args]
    (let [flags (hash-set ; :hover-search
                          :search-guide
                          ; :solve-hover
                          ; :brake-integrate
                          ; :solve-brake-4
                          :hover-guide
                          :hover-integrate
                          )]
      (if (flags flag) (apply println args)))))

(defn- debugln [& args] nil)

(defrecord Control [^long angle ^long power])

(defrecord Lander [^double x
                   ^double y
                   ^double vx
                   ^double vy
                   ^long fuel
                   ^Control control])

(defrecord Stage [stage
                  ^boolean left?
                  ^geometry.Section section 
                  ^geometry.Section pad
                  ^double x-goal
                  ^double x-pad
                  ^double y-pad
                  surface])

(defrecord Constraint [^double dx 
                       ^double dh
                       ^double t])

(defrecord Roots [^double left
                  ^double right])

(defrecord Move [state
                 ^Lander lander
                 ^double dt])

(defn- control-to [f t]
  (let [angle-max-delta 15
        power-max-delta 1
        tune-value (fn [current goal max-delta]
                     (let [delta (- goal current)]
                       (cond (= 0 delta) goal
                             (< 0 delta) (if (< delta max-delta) goal (+ current max-delta))
                             (> 0 delta) (if (> delta (- max-delta)) goal (- current max-delta)))))]
    (->Control (tune-value (:angle f) (:angle t) angle-max-delta)
               (tune-value (:power f) (:power t) power-max-delta))))

; Таблица ускорений в зависимости от угла и мощности. В рассчёте учитываем, что
; угол задаётся от оси (+ PI/2)

(let [M 3.711
      cos (fn [a] (Math/cos (Math/toRadians (+ 90 a))))
      sin (fn [a] (Math/sin (Math/toRadians (+ 90 a))))
      x-force (fn [a p] (* p (cos a)))
      y-force (fn [a p] (- (* p (sin a)) M))
      make-table (fn [f] (vec (for [p (range 0 5)] (vec (for [a (range -90 91)] (f a p))))))
      x-table (make-table x-force)
      y-table (make-table y-force)]
  (defn- x-acceleration [a p] (nth (nth x-table p) (+ 90 a)))
  (defn- y-acceleration [a p] (nth (nth y-table p) (+ 90 a))))

; Движение модуля l при управлении (vec angle power). Сохраняем новое положение
; модуля и то управление, которое привело его в это положение. Положение -
; вектор в фазовом пространстве (vec x y dx dy fuel)
 
(defn move [tc t {lc :control :as l}] 
  (assert (or (= 1.0 t) (= tc lc)))

  (let [{angle :angle power :power :as nc} (control-to lc tc)
        ax (x-acceleration angle power)
        ay (y-acceleration angle power)
        {x :x y :y vx :vx vy :vy fuel :fuel} l]
    (->Lander (+ x (* vx t) (* 0.5 ax t t))
              (+ y (* vy t) (* 0.5 ay t t))
              (+ vx (* ax t))
              (+ vy (* ay t))
              (- fuel (* power t))
              nc)))

; Облако возможных управлений

(let [power-cloud (range 0 5)
      angle-delta 5
      angle-cloud (range -90 91 angle-delta)]
  (def ^:const control-cloud (for [p power-cloud a angle-cloud] (->Control a p))))

; Проверки того, что модель над поверхностью Марса

(defn- in-range? [{x :x} {ax :ax bx :bx}]
  (and (<= ax x) (< x bx)))

(defn- over-line? [{x :x y :y} {ax :ax ay :ay k :k}]
  (let [min-height 32.0
        rx (- x ax)
        ry (- y ay)]
    (< (+ (* k rx) min-height) ry)))

(defn over-section? [lander section]
  (and (in-range? lander section)
       (over-line? lander section)))

(defn alive? [surface {x :x y :y :as lander}]
  (let [x-max (- 7000.0 1.0)
        y-max (- 3000.0 1.0)]
    (and (<= 0 x x-max)
         (<= 0 y y-max)
         (over-line? lander (first (filter (partial in-range? lander) surface))))))

; Рассчёты для стадии последнего снижения: погашение вертикальной скорости с
; управлением (vec 0 4) 

; (def ^:private ^:const max-vy 38.0)

(defn- reserve [x r] (+ x (* x r)))

; Вычисляем разницу высот, на которой можем погасить vy, полагая, что управление
; уже (vec 0 4). Вычисления в обычной системе координат: Марс внизу. Если
; скорость такая, что гасить её не надо, нас это устраивает и мы отвечаем, что
; не нужна высота и время на сжигание топлива.

(defn- descend-constraint [{vy :vy}]
  (let [max-vy 38.0
        ve (- max-vy)
        ay (y-acceleration 0 4)
        t  (/ (- ve vy) ay)]
    (if (< t 0.0)
      (->Constraint 0.0 0.0 0.0)
      (->Constraint 0.0 (+ (* vy t) (* 0.5 ay ay t)) t))))

(defn- brake-constraint [{vx :vx vy :vy} {left? :left?}]
  (let [xi (if left? 90 -90)
        ay (y-acceleration xi 4)
        ax (x-acceleration xi 4)
        t  (/ (- vx) ax)]
    (if (<= 0.0 t)
      (->Constraint (+ (* vx t) (* 0.5 ax ax t)) 
                    (+ (* vy t) (* 0.5 ay ay t))
                    t)))) 

; (def ^:private ^:const h-constraint-reserve 0.125)
; (def ^:private ^:const dx-constraint-reserve 0.125)
 
(defn- constraint [{x :x h :y fuel :fuel :as l}
                   {xp :x-pad yp :y-pad left? :left? :as S}]
  (let [dh-reserve 0.125
        dx-reserve 0.125
        dc (descend-constraint l)
        bc (brake-constraint l S)
        x-cmp (if left? < >)]
    (if (and dc bc)
      (let [xr (+ x (reserve (:dx bc) dx-reserve))
            hr (+ h (reserve (+ (:dh bc) (:dh dc)) dh-reserve))]
        (and (x-cmp xr xp)
             (< yp hr)
             (< (* 4.0 (+ (:t bc) (:t dc))) fuel))))))

(defn- braking-stage [{x :x vx :vx :as lander}
                      {ax :ax ay :ay bx :bx :as pad}
                      stages]
  (if (and (= 0 vx) (in-range? lander pad))
    stages
    (let [left? (or (< x ax) (< 0.0 vx))
          xp (if left? bx ax)]
      (cons (->Stage :braking left? pad pad xp xp ay nil) stages))))

(defn- hover-stages [{x :x}
                     {ax :ax ay :ay bx :bx :as pad}
                     l-rock r-rock stages] 
  (let [on-left (fn [s] (if (< x (:bx s) bx) (->Stage :hover true s pad (:bx s) bx ay nil)))
        on-right (fn [s] (if (> x (:ax s) ax) (->Stage :hover false s pad (:ax s) ax ay nil)))]
    (concat (cond (< x ax) (keep on-left l-rock)
                  (> x bx) (keep on-right (reverse r-rock)))
            stages)))

(defn- reverse-stage [{x :x vx :vx :as lander}
                          {ax :ax ay :ay bx :bx :as pad}
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

(defn- descend-stage [{x :x} {ax :ax ay :ay bx :bx :as pad}]
  (let [left? (< x ax)
        xp (if left? bx ax)]
    (list (->Stage :descending left? pad pad xp xp ay nil))))

(defn detect-stages [l l-rock pad r-rock]
  (->> (descend-stage l pad)
       (braking-stage l pad)
       (hover-stages l pad l-rock r-rock)
       (reverse-stage l pad l-rock r-rock)))

(defn- solve-square-equation [a b c]
  (if (= 0.0 a)
    (if (not= 0.0 b)
      (let [t (/ (- c) b)] (->Roots t t)))
    
    (let [D (- (* b b) (* 4.0 a c))]
      (if (<= 0.0 D)
        (let [D-sqrt (Math/sqrt D)
              a-rcpr (/ (* 2.0 a))
              tp     (* (+ (- b) D-sqrt) a-rcpr)
              tm     (* (- (- b) D-sqrt) a-rcpr)]
          (->Roots (min tp tm) (max tp tm)))))))

(defn solve-hover [{x :x vx :vx {a :angle p :power :as lc} :control :as l}
                   {x-target :x-goal}]
  (let [ax (x-acceleration a p)
        r  (solve-square-equation (* 0.5 ax) vx (- x x-target))]
    (if r 
      (let [{tl :left tr :right} r
            tta (if (<= 0.0 tl) tl tr)]
        (if (<= 0.0 tta) 
          (->Move :ok (move lc tta l) tta))))))

(defn hover-align-control [{section :section :as stage} lander ctl]
  (loop [l lander t 0.0]
    (cond (not (over-line? l section)) nil ; FIXME: Теряем некоторые траектории  
          (not (in-range? l section))  (if (constraint l stage) (->Move :out l t))   
          (= ctl (:control l))         (->Move :ok l t)
          :else                        (recur (move ctl 1.0 l) (+ 1.0 t)))))

(defn- hover-ok-integrate-one [{section :section :as stage} {t :dt lander :lander}]
  (if-let [m (solve-hover lander stage)]
    (let [{tta :dt l :lander} m]
      (if (and (constraint l stage)
               (over-line? l section))
        (let [t-ceil    (Math/ceil tta)
              t-overall (+ t t-ceil)]
          (->Move :ok (move (:control l) t-ceil lander) t-overall))))))

(defn hover-integrate-ok-one [{section :section :as stage} lander]
  (if-let [m (solve-hover lander stage)]
    (let [t (Math/ceil (:dt m))
          l (move (:control lander) t lander)]
      (if (and (constraint l stage)
               (over-line? l section))
        (->Move :ok l t)))))

(defn- hover-out-integrate [moves]
  (map (fn [[k v]] (first v)) (group-by (comp :control :lander) moves)))

(defn- hover-move-cloud [stage lander]
  (let [aligned (group-by :state (keep (partial hover-align-control stage lander) control-cloud))]
    (concat (keep (partial hover-ok-integrate-one stage) (:ok aligned))
            (hover-out-integrate (:out aligned)))))

(defrecord ^:private HoverReduce [moves outs])

(defn hover-integrate [stage lander control]
  (if-let [ma (hover-align-control stage lander control)]
    (case (:state ma)
      :ok (if-let [mi (hover-integrate-ok-one stage (:lander ma))] (vector mi))
      :out (vector ma))))

(defn hover-cloud [stage lander]
  (loop [outs (transient (hash-set)) moves (transient (vector)) controls control-cloud]
    (if-not (empty? controls)
      (do (if-let [m (hover-integrate stage lander (first controls))]
            (let [fm (first m)]
              (case (:state fm)
                :ok  (conj! moves m)
                :out (when-not (outs (:control (:lander fm)))
                       (conj! moves m)
                       (conj! outs (:control (:lander fm)))))))
          (recur outs moves (rest controls))
          )
      (persistent! moves))))

(declare search-guide)

(defn- hover-guide-old [stage next-stages lander]
  (loop [landers (map (comp :lander last) (hover-cloud stage lander))]
    (if-not (empty? landers)
      (let [{ctl :control :as l} (first landers)
            ctl-next (search-guide next-stages l)]
        (if ctl-next
          (cons ctl ctl-next)
          (recur (next landers)))))))

(defn- hover-guide [stage next-stages lander]
  (debugln :hover-guide \newline "stage:" stage \newline "lander:" lander)
  (loop [controls (hover-cloud stage lander)]
    (debugln :hover-guide (count controls))
    (if-let [m (first controls)]
      (do (debugln :hover-guide m \newline (:lander (last m)))
          (let [m-next (search-guide next-stages (:lander (last m)))]
            (if m-next
              (cons m m-next)
              (recur (next controls))))))))

(defn- brake-align-control [lander {pad :pad} ctl]
  (loop [l lander t 0.0]
    (cond (not (over-section? l pad)) nil
          (= ctl (:control l))        (->Move :ok l t)
          :else                       (recur (move ctl 1.0 l) (+ 1.0 t)))))

; Торможение - это 4 стадии: (1) торможение с переходом к выбранному контролю;
; (2) торможение с выбранным контролем; (3) торможение во время перехода к
; (контроль 0 4); (4) выравнивание последней скорости. Поэтому несколько
; движений. Решение на стадии (2) можно принимать лишь после моделирования
; стадии 3. Поэтому порядок таков. Кажется, движение аддитивно, поэтому можно
; обойтись без дополнительных повторных рассчётов.  Вроде как, считать не долго,
; поэтому ограничения на высоту проверяем в самом конце.

; Решение для стадии (2) торможения. Тонкости. (2.1) считаем, что целевая
; vx равна 0. (2.2) считаем, что должны хотя бы 0 секунд тормозить. Иначе, нам
; дали плохой контроль, и можно было бы потратить меньше топлива на остановку.

(defn- solve-brake-2 [{vx :vx {angle :angle power :power :as ctl} :control :as lander}
                      {pad :pad :as stage}]
  (let [ax (x-acceleration angle power)
        tb (/ (- vx) ax)]
    (if (<= 0.0 tb)
      (let [t (Math/ceil tb)
            l (move ctl t lander)]
        (if (over-section? l pad)
          (->Move :ok l t))))))

; Решение для стадии (4) торможения. Тонкости. (4.1) ищем такой угол a для
; (контроль a 4), который позволит погасить остаточную скорость, оставаясь в
; границах pad.

(defn- solve-brake-4 [{x :x vx :vx :as lander}
                      {pad :pad :as stage}]
  (let [tx (if (< 0.0 vx) (:bx pad) (:ax pad))]
    (if (not= x tx)
      (let [dx  (- tx x)
            ax  (- (* vx vx 0.5 (/ dx)))
            t   (Math/ceil (/ vx ax))
            axt (/ vx t)]
        (debugln :solve-brake-4 "vx:" vx "dx:" dx "ax:" ax "(t axt):" (list t axt))
        (if (> axt 0.0)
          (if (<= axt (x-acceleration -15 4))
            (let [a     (- (Math/toDegrees (Math/acos (/ axt 4.0))) 90.0)
                  angle (max -15 (Math/floor a))
                  ctl   (->Control angle 4)]
              (->Move :ok (move ctl t (assoc lander :control ctl)) t)))

          (if (>= axt (x-acceleration 15 4))
            (let [a     (- (Math/toDegrees (Math/acos (/ axt 4.0))) 90.0)
                  angle (min 15 (Math/ceil a))
                  ctl   (->Control angle 4)]
              (->Move :ok (move ctl t (assoc lander :control ctl)) t))))))))

(defn brake-integrate [{pad :pad :as stage} lander ctl]
  (debugln :brake-integrate "brake-integrate hi!" \newline "pad:" pad \newline "lander:" lander)
  (if-let [m-1 (brake-align-control lander stage ctl)]
    (do (debugln :brake-integrate "m-1:" m-1)
        (if-let [m-3 (brake-align-control (:lander m-1) stage (->Control 0 4))]
          (do (debugln :brake-integrate "m-3:" m-3)
              (if-let [m-2 (solve-brake-2 (assoc (:lander m-3) :control ctl) stage)]
                (do (debugln :brake-integrate "m-2:" m-2)
                    (if-let [m-4 (solve-brake-4 (:lander m-2) stage)]
                      (do (debugln :brake-integrate "m-4:" m-4)
                          (if-let [dc (descend-constraint (:lander m-4))]
                            (let [dh-reserve 0.125
                                  hr (+ (:y lander) (reserve (:dh dc) dh-reserve))]
                              (if (and (< (:ay pad) hr)
                                       (< (* 4.0 (:t dc)) (:fuel (:lander m-4))))
                                [m-1 m-2 m-3 m-4])))))))))))) 

(defn break-guide [stage lander] (list))

(defn search-guide [stages ^Lander lander]
  (debugln :search-guide "search guide" (:stage (first stages)))
  (if-let [s (first stages)]
    (case (:stage s)
;      :breaking (break-guide s lander)
      :hover (hover-guide s (next stages) lander)
      (list))))

(defn model-hover [^Control control ^Stage stage ^Lander lander]
  (let [x-goal (:x-goal stage)
        ctl-move (partial move control 1.0)
        on-stage? (if (:left? stage)
                    (fn [l] (< (:x l) x-goal))
                    (fn [l] (>= (:x l) x-goal)))]
    (loop [l (ctl-move lander) L [lander]]
      (if (on-stage? l)
        (recur (ctl-move l) (conj L l))
        (conj L l)))))

(defn model-control [controls stages ^Lander lander]
  (->> (map list stages controls)
       (reductions (fn [trace [s c]] (model-hover c s (last trace))) [lander])))
