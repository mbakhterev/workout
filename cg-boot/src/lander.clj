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

(defn control-match? [angle power ^lander.Lander {a :angle p :power}]
  (and (= a angle) (= p power)))

(defn- x-acceleration [angle power] (* power (x-power angle)))
(defn- y-acceleration [angle power] (- (* power (y-power angle)) M))

(defn move [control-angle control-power t ^lander.Lander {x :x y :y vx :vx vy :vy fuel :fuel :as l}] 
  (comment (println "moving lander:" l))
  (let [m (control-match? control-angle control-power l)]
    (if-not (or m (= 1.0 t))
      (throw (Exception. (format "cannot move that: %.3f %b. ctl: %d %d. lnd: %d %d"
                                 t m
                                 control-angle control-power
                                 (:angle l) (:power l))))))

  (let [angle (control-to (:angle l) control-angle (* t angle-max-delta))
        power (control-to (:power l) control-power (* t power-max-delta))
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

(defn- over-section? [^lander.Lander {x :x y :y} ^geometry.Section {ax :ax bx :bx ay :ay k :k}]
  (and (<= ax x bx)
       (let [rx (- x ax)
             ry (- y ay)] (<= ry (* k rx)))))

(defn alive? [surface ^lander.Lander {x :x y :y :as l}]
  (and (<= 0 x x-max)
       (<= 0 y y-max)
       (not (some (partial over-section? l) surface))))

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

(defn- reserve [x r] (+ x (* x r)))

; Вычисляем разницу высот, на которой можем погасить vy, полагая, что управление
; уже (vec 0 4). Вычисления в обычной системе координат: Марс внизу. Если
; скорость такая, что гасить её не надо, нас это устраивает и мы отвечаем, что
; не нужна высота и время на сжигание топлива.

(defn descending-constraint-h [^lander.Lander {vy :vy}]
  (let [ve (- max-vy)
        ay (- 4.0 M)
        t  (/ (- ve vy) ay)]
    (if (< t 0)
      [true 0.0 0.0]
      [true (+ (* vy t) (* 0.5 ay ay t)) t])))

(defn- braking-constraint-core [x vx ax vy]
  (let [t  (/ (- vx) ax)
        ay (- M)]
    (if (< t 0)
      [false 0.0 0.0 t]
      [true (+ (* vy t) (* 0.5 ay ay t))
            (+ (* vx t) (* 0.5 ay ay t))
            t]))) 

(defn braking-constraint-h-dx [^lander.Lander {x :x vx :vx vy :vy}
                               ^geometry.Section {ax :ax bx :bx}]
  (cond (< x ax) (braking-constraint-core x vx -4.0 vy)
        (> x bx) (braking-constraint-core x vx +4.0 vy)
        :else    (braking-constraint-core x vx (if (>= vx 0.0) -4.0 +4.0) vy)))

(defn constraint [^lander.Lander {x :x vx :vx h :y fuel :fuel :as l}
                  ^geometry.Section {ax :ax bx :bx ay :ay :as landing-pad}]
  (let [[descend-ok descend-h descend-t :as dc] (descending-constraint-h l)
        [brake-ok brake-h brake-dx brake-t :as bc] (braking-constraint-h-dx l landing-pad)
        dx (cond (< x ax)   (- bx x)
                 (< bx x)   (- ax x)
                 (> 0.0 vx) (- bx x)
                 :else      (- ax x))]
    (comment (println "dc:" dc "bc:" bc "dx:" dx))
    (and brake-ok 
         descend-ok
         (>= dx (reserve brake-dx dx-constraint-reserve))
         (<= ay (+ h (reserve (+ brake-h descend-h) h-constraint-reserve)))
         (<= (* 4.0 (+ descend-t brake-t)) fuel))))

(defn- add-braking-stage [x vx ax bx stage]
  (if (and (= 0 vx) (< ax x bx)) stage (cons {:stage :braking} stage)))

(defn- add-hover-stages [x ax bx l-rock r-rock stage] 
  (let [on-left (fn [s] (if (< x (:bx s) bx) {:stage :hover
                                              :direction :right
                                              :section s}))

        on-right (fn [s] (if (> x (:ax s) ax) {:stage :hover
                                               :direction :left
                                               :section s}))]

    (concat (cond (< x ax) (keep on-left l-rock)
                  (> x bx) (keep on-right (reverse r-rock)))
            stage)))

(defn- add-reverse-stage [x vx ax bx stage]
  (if (or (and (< x ax) (< vx 0.0))
          (and (> x bx) (> vx 0.0)))
    (cons {:stage :reverse} stage)
    stage))

(defn detect-stages [^lander.Lander {x :x vx :vx} l-rock ^geometry.Section {ax :ax bx :bx} r-rock]
  (->> (list {:stage :descending})
       (add-braking-stage x vx ax bx)
       (add-hover-stages x ax bx l-rock r-rock)
       (add-reverse-stage x vx ax bx)))

(defn- solve-square-equation [a b c]
  (if (= 0.0 a)
    (if (= 0.0 b) [false 0.0 0.0] (let [x (/ (- c) b)] [true x x]))
    
    (let [D (- (* b b) (* 4 a c))]
      (if (< D 0.0)
        [false 0.0 0.0]
        (let [D-sqrt (Math/sqrt D)
              a-rcpr (/ (* 2.0 a))
              tp     (* (+ (- b) D-sqrt) a-rcpr)
              tm     (* (- (- b) D-sqrt) a-rcpr)]
          [true (min tp tm) (max tp tm)])))))

(defn- hover?
  [{^geometry.Section {ax :ax bx :bx ay :ay by :by k :k} :section} ^lander.Lander {x :x y :y}]
  (and (<= ax x bx)
       (> (- y ay) (* k (- x ax)))
       (<= 0 x x-max)
       (<= 0 y y-max)))

(defn solve-hover [^lander.Lander {x :x vx :vx angle :angle power :power :as l} target-x]
  (let [ax         (x-acceleration angle power)
        [ok tl tr] (solve-square-equation (* 0.5 ax) vx (- x target-x))]
    (if-let [tta (and ok (if (<= 0.0 tl tr) tl (if (<= 0.0 tr) tr)))]
      [true (move angle power tta l) tta]
      [false nil 0.0])))

; Здесь проблема с модулями, которые не успевают стабилизировать свой контроль
; до границы отрезка. Чтобы точно их моделировать, надо внести поправку перед
; solve-рассчётом в угол и мощность. Кроме этого, для проверки надо накопить
; общее время полёта. Чтобы не порождать дополнительные траектории разумно
; запоминать уже пройденные. Структура отображения (экспериментальный синтаксис)
;
;   (type -> (tuple angle power) (tuple ok lander time))
;
; После этого надо проанализировать lander на ограничения, и вернуть позицию во
; время (ceiling time). (move a p (ceiling time) l) будет в области действия
; следующего сегмента, поэтому необходимо предварительно проверить входящие
; точки на ограничения. По высоте и скорости тоже. Но, видимо, их разумнее
; отфильтровать в другом месте

; FIXME: проверки на точные равенства - потенциальный источник больших проблем.
; Но пока работа над общей схемой.

(defn- approach-loop [^lander.Lander lander ^geometry.Section {ax :ax bx :bx :as section} angle power]
  (loop [l lander t 0.0]
    (if (control-match? angle power l)
      [:ok l t]
      (let [l-next (move angle power 1.0 l)]
        (cond (not (<= ax (:x l-next) bx))   [:out l t]
              (over-section? l-next section) [:ko l t] 
              :else                          (recur l-next (+ 1.0 t)))))))

(defn- trace-hover [^geometry.Section landing-pad
                    traces
                    target-x
                    ^lander.Lander {angle :angle power :power :as lander}
                    t]
  (if (traces [angle power])
    traces
    (let [[ok l tta] (solve-hover lander target-x)]
      (comment (println l (constraint l landing-pad)))
      (if-not (and ok (constraint l landing-pad))
        traces
        (let [t-int     (Math/ceil tta)
              t-overall (+ t t-int)]
          (assoc traces [angle power] [true (move angle power t-int lander) t-overall])))))) 

(defn integrate-hover [^geometry.Section landing-pad
                       {direction :direction ^geometry.Section {bx :bx ax :ax :as S} :section}
                       ^lander.Lander lander
                       traces
                       angle
                       power]
  (comment (println angle power))
  (let [target-x (if (= :right direction) bx ax)
        [state lA t] (approach-loop lander S angle power)]
    (comment (println state target-x lA t))
    (case state
      :ko  traces
      :ok  (trace-hover landing-pad traces target-x lA t)
      :out (let [lC (assoc lA :angle (:angle lA) :power (:power lA))]
             (trace-hover landing-pad traces target-x lC t)))))

; Это общая схема, которая может пригодится для разных стадий

(defn integrate-wrap [f landing-pad stage lander]
  (fn [traces [angle power]] (f landing-pad stage lander traces angle power)))
