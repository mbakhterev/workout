(ns lander (:require [render :as r]
                     [geometry :refer :all]))

(defrecord Lander [^double x
                   ^double y
                   ^double vx
                   ^double vy
                   ^long fuel
                   ^long angle
                   ^long power
                   ^boolean alive]) 

(set! *warn-on-reflection* true)

; Синусы и косинусы для рассчёта проекции тяги. Угол задаётся от оси (+ PI/2).
; Симметричность cos не учитывается, чтобы не усложнять формулу пересчёта угла phi
; в индекс таблицы i. Формула должна быть такой i = phi + 90

(def ^:private ^:const ^doubles cos-table
  (mapv (fn [d] (Math/cos (Math/toRadians (+ d 90)))) (range -90 91)))

(def ^:private ^:const ^doubles sin-table
  (mapv (fn [d] (Math/sin (Math/toRadians (+ d 90)))) (range -90 91)))

; Функции для доступа в таблицы по значению угла

(defn- x-power [^long phi] (nth cos-table (+ phi 90)))
(defn- y-power [^long phi] (nth sin-table (+ phi 90)))

; Движение модуля l при управлении (vec angle power). Сохраняем новое положение
; модуля и то управление, которое привело его в это положение. Положение -
; вектор в фазовом пространстве (vec x y dx dy fuel). Нужно быстро считать,
; поэтому juxt не используем.

(def ^:private ^:const M 3.711)  

(defn- move [l angle power]
  (if (not (:alive l))
    l
    (let [t    1.0
          x    (:x l)
          y    (:y l)
          vx   (:vx l)
          vy   (:vy l)
          fuel (:fuel l)
          ax   (* power (x-power angle))
          ay   (- (* power (y-power angle)) M)]
    (->Lander (+ x (* vx t) (* 0.5 ax t t))
              (+ y (* vy t) (* 0.5 ay t t))
              (+ vx (* ax t))
              (+ vy (* ay t))
              (- fuel (* power t))
              angle
              power
              true))))

(defn- move-back [l angle power]
  (if (not (:alive l))
    l
    (let [t    1.0
          x    (:x l)
          y    (:y l)
          vx   (:vx l)
          vy   (:vy l)
          fuel (:fuel l)
          ax   (* power (x-power angle))
          ay   (- (* power (y-power angle)) M)]
      (->Lander (- x (* vx t) (* 0.5 ax t t))
                (- y (* vy t) (* 0.5 ay t t))
                (- vx (* ax t))
                (- vy (* ay t))
                (+ fuel (* power t))
                angle
                power
                true))))

(defn- wrap [f] (fn [l [a p]] (f l a p)))

(def ^:private ^:const ^double x-max (- 7000.0 1.0))
(def ^:private ^:const ^double y-max (- 3000.0 1.0))

(defn- ^boolean alive? [surface ^records.Lander l]
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

(defn- path-cloud [path]
  (let [l (first path)]
    (for [p (nth power-cloud (:power l)) a (angle-cloud (:angle l))] 
      (cons (move-back l a p) path))))


