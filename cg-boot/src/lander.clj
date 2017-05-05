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

(defn ^Lander load-lander [nums] (let [[m c] (split-at 5 nums)] (apply ->Lander (conj (vec m) (apply ->Control c))))) 

(defrecord Stage [stage
                  ^boolean left?
                  ^geometry.Section section 
                  ^geometry.Section pad
                  ^double x-goal
                  ^double x-pad
                  ^double y-pad
                  surface])

(defrecord Constraint [^double dx ^double dh ^double t])

(defrecord Roots [^double left ^double right])

(defrecord Move [state ^Lander lander ^double dt])

(defn- control-to [^Control f ^Control t]
  (let [angle-max-delta 15
        power-max-delta 1

        tune-value (fn [^long current ^long goal ^long max-delta]
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
      x-force (fn ^double [a p] (* p (cos a)))
      y-force (fn ^double [a p] (- (* p (sin a)) M))
      make-table (fn [f] (vec (for [p (range 0 5)] (vec (for [a (range -90 91)] (f a p))))))
      x-table (make-table x-force)
      y-table (make-table y-force)]
  (defn- x-acceleration [^long a ^long p] ((x-table p) (+ 90 a)))
  (defn- y-acceleration [^long a ^long p] ((y-table p) (+ 90 a))))

; Движение модуля l при управлении (vec angle power). Сохраняем новое положение
; модуля и то управление, которое привело его в это положение. Положение -
; вектор в фазовом пространстве (vec x y dx dy fuel)
 
(defn- move [^Control ctl
             ^double t
             ^Lander {lc :control :as l}] 
  (assert (or (= 1.0 t) (= lc ctl)))

  (let [{angle :angle power :power :as nc} (control-to lc ctl)
        ax (x-acceleration angle power)
        ay (y-acceleration angle power)
        {x :x y :y vx :vx vy :vy fuel :fuel lt :dt} l]
    (->Lander (+ x (* vx t) (* 0.5 ax t t))
              (+ y (* vy t) (* 0.5 ay t t))
              (+ vx (* ax t))
              (+ vy (* ay t))
              (- fuel (* power t))
              nc)))

; Облако возможных управлений

(def ^:const ^:power angle-delta 5)

(let [power-cloud (range 0 5)
      angle-cloud (range -90 91 angle-delta)]
  (def ^:const control-cloud (vec (for [p power-cloud a angle-cloud] (->Control a p)))))

; Проверки того, что модель над поверхностью Марса

(defn- in-range? [^Lander {x :x} ^geometry.Section {ax :ax bx :bx}]
  (and (<= ax x) (< x bx)))

(defn- over-line? [^Lander {x :x y :y} ^geometry.Section {ax :ax ay :ay k :k}]
  (let [min-height 32.0
        rx (- x ax)
        ry (- y ay)]
    (< (+ (* k rx) min-height) ry)))

(defn- over-section? [^Lander lander ^geometry.Section section]
  (and (in-range? lander section)
       (over-line? lander section)))

(defn alive? [^geometry.Section surface ^Lander {x :x y :y :as lander}]
  (let [x-max (- 7000.0 1.0)
        y-max (- 3000.0 1.0)]
    (and (<= 0 x x-max)
         (<= 0 y y-max)
         (over-line? lander (first (filter (partial in-range? lander) surface))))))

; Рассчёты для стадии последнего снижения: погашение вертикальной скорости с
; управлением (vec 0 4) 

(defn- reserve [^double x ^double r] (+ x (* x r)))

; Вычисляем разницу высот, на которой можем погасить vy, полагая, что управление
; уже (vec 0 4). Вычисления в обычной системе координат: Марс внизу. Если
; скорость такая, что гасить её не надо, нас это устраивает и мы отвечаем, что
; не нужна высота и время на сжигание топлива.

(defn- descend-constraint [^Lander {vy :vy}]
  (let [max-vy 38.0
        ve (- max-vy)
        ay (y-acceleration 0 4)
        t  (/ (- ve vy) ay)]
    (if (< t 0.0)
      [0.0 0.0]
      [(+ (* vy t) (* 0.5 ay ay t)) t])))

(defn- brake-constraint [^Lander {vx :vx vy :vy} ^Stage {left? :left?}]
  (let [xi (if left? 90 -90)
        ay (y-acceleration xi 4)
        ax (x-acceleration xi 4)
        t  (/ (- vx) ax)]
    (if (<= 0.0 t)
      [(+ (* vx t) (* 0.5 ax ax t)) 
       (+ (* vy t) (* 0.5 ay ay t))
       t]))) 

(defn- constraint [^Lander {x :x h :y fuel :fuel :as l}
                   ^Stage {xp :x-pad yp :y-pad left? :left? :as S}]
  (if-let [bc (brake-constraint l S)]
    (let [dh-reserve 0.125
          dx-reserve 0.125
          [bc-x bc-h bc-t] bc
          [dc-h dc-t] (descend-constraint l)
          x-cmp (if left? < >)
          xr (+ x (reserve bc-x dx-reserve))
          hr (+ h (reserve (+ bc-h dc-h) dh-reserve))]
      (and (x-cmp xr xp)
           (< yp hr)
           (< (* 4.0 (+ bc-t dc-t)) fuel)))))

(defn- braking-stage [^Lander {x :x vx :vx :as lander}
                      ^geometry.Section {ax :ax ay :ay bx :bx :as pad}
                      stages]
  (if (and (= 0 vx) (in-range? lander pad))
    stages
    (let [left? (or (< x ax) (< 0.0 vx))
          xp (if left? bx ax)]
      (cons (->Stage :braking left? pad pad xp xp ay nil) stages))))

(defn- hover-stages [^Lander {x :x}
                     ^geometry.Section {ax :ax ay :ay bx :bx :as pad}
                     l-rock r-rock stages] 
  (let [on-left (fn [s] (if (< x (:bx s) bx) (->Stage :hover true s pad (:bx s) bx ay nil)))
        on-right (fn [s] (if (> x (:ax s) ax) (->Stage :hover false s pad (:ax s) ax ay nil)))]
    (concat (cond (< x ax) (keep on-left l-rock)
                  (> x bx) (keep on-right (reverse r-rock)))
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
    (list (->Stage :descending left? pad pad xp xp ay nil))))

(defn detect-stages [l l-rock pad r-rock]
  (->> (descend-stage l pad)
       (braking-stage l pad)
       (hover-stages l pad l-rock r-rock)
       (reverse-stage l pad l-rock r-rock)))

(defn- solve-square-equation [^double a ^double b ^double c]
  (if (= 0.0 a)
    (if (not= 0.0 b)
      (let [t (/ (- c) b)] [t t]))
    
    (let [D (- (* b b) (* 4.0 a c))]
      (if (<= 0.0 D)
        (let [D-sqrt (Math/sqrt D)
              a-rcpr (/ (* 2.0 a))
              tp     (* (+ (- b) D-sqrt) a-rcpr)
              tm     (* (- (- b) D-sqrt) a-rcpr)]
          [(min tp tm) (max tp tm)])))))

(defn- solve-hover [^Lander {x :x vx :vx a :angle p :power :as l}
                    ^Stage {x-target :x-goal}]
  (let [ax (x-acceleration a p)
        r  (solve-square-equation (* 0.5 ax) vx (- x x-target))]
    (if r 
      (let [[tl tr] r
            tta (if (<= 0.0 tl) tl tr)]
        (if (<= 0.0 tta) 
          (move a p (Math/ceil tta) l))))))

(defn hover-align-control [^Stage {section :section :as stage} ^Lander lander ^long angle ^long power]
  (loop [l (assoc lander :dt 0.0)]
    (cond (not (over-line? l section))   (assoc l :state :ko)
          (not (in-range? l section))    (if (constraint l stage) (assoc l :state :out) (assoc l :state :ko))
          (control-match? angle power l) l
          :else                          (recur (move angle power 1.0 l)))))

(defn hover-integrate-ok-one [^Stage {section :section :as stage} ^Lander lander]
  (if-let [l (solve-hover (assoc lander :dt 0.0) stage)]
    (and (constraint l stage)
         (over-line? l section)
         l)))

(defn hover-integrate [^Stage stage
                        ^Lander lander
                        ^long angle ^long power]
  (let [{state :state :as la} (hover-align-control stage lander angle power)]
    (case state
      :ko nil
      :ok (if-let [li (hover-integrate-ok-one stage la)] (list li la))
      :out (list la))))

(defn hover-cloud [^Stage stage ^Lander lander]
  (keep identity (for [p (range 0 5)
                       a (let [a-left (:angle (hover-align-control stage lander -90 p))
                               a-right (:angle (hover-align-control stage lander 90 p))]
                           (if-not (:left? stage)
                             (range a-left (+ a-right 1) angle-delta)
                             (range a-right (- a-left 1) (- angle-delta))))]
                   (hover-integrate stage lander a p))))

(declare search-guide)

(defn hover-guide [^Stage stage next-stages ^Lander lander]
  (loop [cloud (hover-cloud stage lander)]
    (when-first [l cloud]
      (debugln :hover-guide l)
      (if-let [l-next (search-guide next-stages (first l))]
        (cons l l-next)
        (recur (next cloud))))))

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
      :hover (hover-guide s (rest stages) lander)
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
