(ns lander (:require [geometry :refer :all]))

(set! *warn-on-reflection* true)

(defrecord Lander [^double x
                   ^double y
                   ^double vx
                   ^double vy
                     ^long fuel
                     ^long angle
                     ^long power])

; Синусы и косинусы для рассчёта проекции тяги. Угол задаётся от оси (+ PI/2).
; Симметричность cos не учитывается, чтобы не усложнять формулу пересчёта угла phi
; в индекс таблицы i. Формула должна быть такой i = phi + 90

(def ^:private ^:const cos-table
  (mapv (fn [d] (Math/cos (Math/toRadians (+ d 90)))) (range -90 91)))

(def ^:private ^:const sin-table
  (mapv (fn [d] (Math/sin (Math/toRadians (+ d 90)))) (range -90 91)))

; Функции для доступа в таблицы по значению угла

(defn- x-power [phi] (nth cos-table (+ phi 90)))
(defn- y-power [phi] (nth sin-table (+ phi 90)))

; Движение модуля l при управлении (vec angle power). Сохраняем новое положение
; модуля и то управление, которое привело его в это положение. Положение -
; вектор в фазовом пространстве (vec x y dx dy fuel)

(def ^:private ^:const M 3.711)  

(def ^:private ^:const angle-max-delta 15.0)
(def ^:private ^:const power-max-delta 1.0)

(defn- control-to [current goal max-delta]
  (let [delta (- goal current)]
    (cond (= 0 delta) goal
          (< 0 delta) (if (< delta max-delta) goal (+ current max-delta))
          (< 0 delta) (if (< delta max-delta) goal (- current max-delta)))))

(defn control-match? [angle power l] (and (= (:angle l) angle)
                                          (= (:power l) power)))

(defn- x-acceleration [angle power] (* power (x-power angle)))
(defn- y-acceleration [angle power] (- (* power (y-power angle)) M))

(defn move [control-angle control-power t l]
  (let [m (control-match? control-angle control-power l)]
    (if (and (not= 1.0 t) (not m))
      (throw (Exception. (format "cannot move that: %.3f %b" t m)))))

  (let [angle (control-to (:angle l) control-angle (* t angle-max-delta))
        power (control-to (:power l) control-power (* t power-max-delta))
        x     (:x l)
        y     (:y l)
        vx    (:vx l)
        vy    (:vy l)
        fuel  (:fuel l)
        ax    (x-acceleration angle power)
        ay    (y-acceleration angle power)]
    (->Lander (+ x (* vx t) (* 0.5 ax t t))
              (+ y (* vy t) (* 0.5 ay t t))
              (+ vx (* ax t))
              (+ vy (* ay t))
              (- fuel (* power t))
              angle
              power)))

(defn wrap [t f] (fn [l [a p]] (f a p t l)))

(def ^:private ^:const x-max (- 7000.0 1.0))
(def ^:private ^:const y-max (- 3000.0 1.0))

(defn alive? [surface l]
  (let [x (:x l)
        y (:y l)]
    (and (<= 0 x x-max)
         (<= 0 y y-max)
         (not (some (fn [s] (and (<= (:ax s) x (:bx s))
                                 (let [rx (- x (:ax s))
                                       ry (- y (:ay s))]
                                   (<= ry (* (:k s) rx)))))
                    surface)))))

(defn- gen-cloud [base cloud array-convert]
  (let [A (first base)
        B (last base)]
    (->> base
         (mapv (fn [p] (->> cloud
                            (map (partial + p))
                            (filter (fn [i] (<= A i B)))
                            array-convert
                            vec))))))

(def ^:private ^:const dA 5)
(def ^:private ^:const angle-cloud-table (gen-cloud (range -90 91)
                                                    (range -15 (+ 15 dA) dA)
                                                    long-array))

(defn- angle-cloud [phi] (nth angle-cloud-table (+ 90 phi)))

(def ^:private ^:const power-cloud (gen-cloud (range 0 5)
                                              (range -1.0 2.0)
                                              double-array))

(comment (defn path-cloud [path]
  (let [l (first path)]
    (for [p (nth power-cloud (:power l)) a (angle-cloud (:angle l))] 
      (cons (move-back l a p) path)))))

; Рассчёты для стадии последнего снижения: погашение вертикальной скорости с
; управлением (vec 0 4) 

(def ^:private ^:const max-vy 38.0)
(def ^:private ^:const h-constraint-reserve 0.125)
(def ^:private ^:const dx-constraint-reserve 0.125)

; Вычисляем разницу высот, на которой можем погасить vy, полагая, что управление
; уже (vec 0 4). Вычисления в обычной системе координат: Марс внизу 

(defn- descending-constraint-h [l]
  (let [vy (:vy l)
        ve (- max-vy)
        ay (- 4.0 M)
        t  (/ (- ve vy) ay)]
    (if (< t 0)
      [:ko 0.0 t]
      [:ok (+ (* vy t) (* 0.5 ay ay t)) t])))

(defn- braking-constraint-core [x vx ax vy]
  (let [t  (/ (- vx) ax)
        ay (- M)]
    (if (< t 0)
      [:ko 0.0 0.0 t]
      [:ok (+ (* vy t) (* 0.5 ay ay t))
           (+ (* vx t) (* 0.5 ay ay t))
           t]))) 

(defn braking-constraint-h-dx [l landing-pad]
  (let [x  (:x l)
        vx (:vx l)
        vy (:vy l)
        ax (:ax landing-pad)
        bx (:bx landing-pad)]
    (cond (< x ax) (braking-constraint-core x vx -4.0 vy)
          (> x bx) (braking-constraint-core x vx +4.0 vy)
          :else    (braking-constraint-core x vx (if (>= vx 0.0) -4.0 +4.0) vy))))

(defn- add-braking-stage [x vx ax bx stage]
  (if (and (= 0 vx) (< ax x bx)) stage (cons {:stage :braking} stage)))

(defn- add-hover-stages [x ax bx l-rock r-rock stage] 
  (let [on-left (fn [s] (if (< x (:bx s) bx) {:stage :hover
                                              :direction :left
                                              :section s}))

        on-right (fn [s] (if (> x (:ax s) ax) {:stage :hover
                                               :direction :right
                                               :section s}))]

    (concat (cond (< x ax) (keep on-left l-rock)
                  (> x bx) (keep on-right (reverse r-rock)))
            stage)))

(defn- add-reverse-stage [x vx ax bx stage]
  (if (or (and (< x ax) (< vx 0.0))
          (and (> x bx) (> vx 0.0)))
    (cons {:stage :reverse} stage)
    stage))

(defn detect-stages [l l-rock pad r-rock]
  (let [x  (:x l)
        vx (:vx l)
        ax (:ax pad)
        bx (:bx pad)]
    (->> (list {:stage :descending})
         (add-braking-stage x vx ax bx)
         (add-hover-stages x ax bx l-rock r-rock)
         (add-reverse-stage x vx ax bx))))

(defn solve-square-equation [a b c]
  (if (= 0.0 a)
    (if (= 0.0 b)
      [false 0.0 0.0] (let [x (/ (- c) b)] [true x x]))
    
    (let [D (- (* b b) (* 4 a c))]
      (if (< D 0.0)
        [false 0.0 0.0]
        (let [D-sqrt (Math/sqrt D)
              a-rcpr (/ (* 2.0 a))
              tp     (* (+ (- b) D-sqrt) a-rcpr)
              tm     (* (- (- b) D-sqrt) a-rcpr)]
          [true (min tp tm) (max tp tm)])))))

(defn- hover? [stage l]
  (let [x  (:x l)
        y  (:y l)
        S  (:section stage)
        k  (:k S)
        ax (:ax S)
        bx (:bx S)
        ay (:ay S)
        by (:by S)]
    (and (<= ax x bx)
         (> (- y ay) (* k (- x ax)))
         (<= 0 x x-max)
         (<= 0 y y-max))))

(defn solve-hover [l target-x]
  (let [x          (:x l)
        vx         (:vx l)   
        angle      (:angle l)
        power      (:power l)
        ax         (x-acceleration angle power)
        [ok tl tr] (solve-square-equation (* 0.5 ax) vx (- x target-x))]
    (println ax vx (- x target-x) ok tl tr)
    (let [tta (and ok (if (<= 0.0 tl tr) tl (if (<= 0.0 tr) tr)))]
      (println tta)
      (if tta [true (move angle power tta l) tta]
              [false nil 0.0]))))

(defn integrate-hover [stage lander angle power]
  (let [stable (loop [l lander R (vector)]
                 (if (or (control-match? angle power l)
                         (not (hover? stage l)))
                   (conj R l)
                   (recur (move angle power 1.0 l)
                          (conj R (move angle power 1.0 l)))))]
    stable))
