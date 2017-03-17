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
; вектор в фазовом пространстве (vec x y dx dy fuel). Нужно быстро считать,
; поэтому juxt не используем.

(def ^:private ^:const M 3.711)  

(def ^:private ^:const angle-max-delta 15.0)
(def ^:private ^:const power-max-delta 1.0)

(defn- control-to [current goal max-delta]
  (let [delta (- goal current)]
    (cond (= 0 delta) goal
          (< 0 delta) (if (< delta max-delta) goal (+ current max-delta))
          (< 0 delta) (if (< delta max-delta) goal (- current max-delta)))))

(defn move [l control-angle control-power]
  (let [angle (control-to (:angle l) control-angle angle-max-delta)
        power (control-to (:power l) control-power power-max-delta)
        t     1.0
        x     (:x l)
        y     (:y l)
        vx    (:vx l)
        vy    (:vy l)
        fuel  (:fuel l)
        ax    (* power (x-power angle))
        ay    (- (* power (y-power angle)) M)]
    (->Lander (+ x (* vx t) (* 0.5 ax t t))
              (+ y (* vy t) (* 0.5 ay t t))
              (+ vx (* ax t))
              (+ vy (* ay t))
              (- fuel (* power t))
              angle
              power)))

(defn- control-match? [l angle power] (and (= (:angle l) angle)
                                           (= (:power l) power))) 

(defn wrap [f] (fn [l [a p]] (f l a p)))

(def ^:private ^:const x-max (- 7000.0 1.0))
(def ^:private ^:const y-max (- 3000.0 1.0))

(defn alive? [surface l]
  (let [x (:x l)
        y (:y l)]
    (and (<= 0 x x-max)
         (<= 0 y y-max)
         (not (some (fn [s] (and (< (:ax s) x (:bx s))
                                 (let [rx (- x (:ax s))
                                       ry (- y (:ay s))]
                                   (<= ry (* (:k s) rx)))))
                    surface)))))

(defn- mark-alive [surface l] (assoc l :alive (alive? surface l)))



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

(defn- add-braking-stage [x vx ax bx S]
  (if-not (and (= 0 vx) (< ax x bx)) (cons {:stage :braking} S) S))

(defn- add-hover-stages [x ax bx l-rock r-rock S] 
  (let [on-left (fn [s] (if (< x (:bx s) bx) {:stage :hover
                                              :direction :left
                                              :section s}))

        on-right (fn [s] (if (> x (:ax s) ax) {:stage :hover
                                               :direction :right
                                               :section s}))]

    (concat (cond (< x ax) (keep on-left l-rock)
                  (> x bx) (keep on-right (reverse r-rock)))
            S)))

(defn- add-reverse-stage [x vx ax bx S]
  (if (or (and (< x ax) (< vx 0.0))
          (and (> x bx) (> vx 0.0)))
    (cons {:stage :reverse} S)
    S))

(defn detect-stages [l l-rock pad r-rock]
  (let [x  (:x l)
        vx (:vx l)
        ax (:ax pad)
        bx (:bx pad)]
    (->> (list {:stage :descending})
         (add-braking-stage x vx ax bx)
         (add-hover-stages x ax bx l-rock r-rock)
         (add-reverse-stage x vx ax bx))))
