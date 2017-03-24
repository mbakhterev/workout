(ns lander (:require [geometry :refer :all]))

(set! *warn-on-reflection* true)

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
                  ^double x-section
                  ^double x-pad
                  ^double y-pad
                  surface])

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

(defn- control-tune [^Control f ^Control t]
  (->Control (control-to (:angle f) (:angle t) angle-max-delta)
             (control-to (:power f) (:power t) power-max-delta)))

(defn- x-acceleration [angle power] (* power (x-power angle)))
(defn- y-acceleration [angle power] (- (* power (y-power angle)) M))

(defn move [^Control tc t ^Lander {lc :control :as l}] 
  (if-not (or (= 1.0 t) (= tc lc))
      (throw (Exception. (format "cannot move that: %.3f %b. ctl: %d %d. lnd: %d %d"
                                 t (= tc lc) (:angle tc) (:power tc) (:angle lc) (:power lc)))))

  (let [{angle :angle power :power :as nc} (control-tune lc tc)
        ax (x-acceleration angle power)
        ay (y-acceleration angle power)
        {x :x y :y vx :vx vy :vy fuel :fuel} l]
    (->Lander (+ x (* vx t) (* 0.5 ax t t))
              (+ y (* vy t) (* 0.5 ay t t))
              (+ vx (* ax t))
              (+ vy (* ay t))
              (- fuel (* power t))
              nc)))

(defn wrap [f t] (fn [^Lander l ^Control c] (f c t l)))

(def ^:private ^:const x-max (- 7000.0 1.0))
(def ^:private ^:const y-max (- 3000.0 1.0))

(defn- over-section? [^Lander {x :x y :y} ^geometry.Section {ax :ax bx :bx ay :ay k :k}]
  (and (<= ax x bx)
       (let [rx (- x ax)
             ry (- y ay)] (<= ry (* k rx)))))

(defn alive? [surface ^Lander {x :x y :y :as l}]
  (and (<= 0 x x-max)
       (<= 0 y y-max)
       (not (some (partial over-section? l) surface))))

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

(defn- descending-constraint-h [^Lander {vy :vy}]
  (let [ve (- max-vy)
        ay (- 4.0 M)
        t  (/ (- ve vy) ay)]
    (if (< t 0)
      [true 0.0 0.0]
      [true (+ (* vy t) (* 0.5 ay ay t)) t])))

(defn- braking-constraint-h-dx [^Lander {vx :vx vy :vy} ^Stage {left? :left?}]
  (let [ax (if left? +4.0 -4.0)
        t  (/ (- vx) ax)
        ay (- M)]
     (if (< t 0)
      [false 0.0 0.0 t]
      [true (+ (* vy t) (* 0.5 ay ay t))
            (+ (* vx t) (* 0.5 ay ay t))
            t]))) 

(defn constraint [^Lander {x :x vx :vx h :y fuel :fuel :as l}
                  ^Stage {xp :x-pad yp :y-pad :as S}]
  (let [[descend-ok descend-h descend-t :as dc] (descending-constraint-h l)
        [brake-ok brake-h brake-dx brake-t :as bc] (braking-constraint-h-dx l S)]
    (comment (println "dc:" dc "bc:" bc "dx:" dx))
    (and brake-ok descend-ok
         (<= (reserve brake-dx dx-constraint-reserve) (Math/abs ^double (- x xp)))
         (<= yp (+ h (reserve (+ brake-h descend-h) h-constraint-reserve)))
         (<= (* 4.0 (+ descend-t brake-t)) fuel))))

(defn- add-braking-stage [^Lander {x :x vx :vx}
                          ^geometry.Section {ax :ax ay :ay bx :bx :as pad} stages]
  (if (and (= 0 vx) (<= ax x bx))
    stages
    (let [left? (or (> x bx) (> 0.0 vx))
          xp (if left? bx ax)]
      (cons (->Stage :braking left? pad pad xp xp ay nil) stages))))

(defn- add-hover-stages [^Lander {x :x}
                         ^geometry.Section {ax :ax ay :ay bx :bx :as pad}
                         l-rock r-rock stages] 
  (let [on-left (fn [s] (if (< x (:bx s) bx) (->Stage :hover true s pad (:bx s) bx ay nil)))
        on-right (fn [s] (if (> x (:ax s) ax) (->Stage :hover false s pad (:ax s) ax ay nil)))]
    (concat (cond (< x ax) (keep on-left l-rock)
                  (> x bx) (keep on-right (reverse r-rock))) stages)))

(defn- add-reverse-stage [^Lander {x :x vx :vx}
                          ^geometry.Section {ax :ax ay :ay bx :bx :as pad}
                          l-rock r-rock stages]
  (if (or (and (< x ax) (< vx 0.0))
          (and (> x bx) (> vx 0.0)))
    (cons {:stage :reverse} stage)
    stage))

(defn detect-stages [^lander.Lander l l-rock ^geometry.Section pad r-rock]
  (->> (list {:stage :descending})
       (add-braking-stage l pad)
       (add-hover-stages l pad l-rock r-rock)
       (add-reverse-stage l pad)))

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

(defn solve-hover
  [^lander.Lander {x :x vx :vx ^lander.Control {a :angle p :power :as lc} :control :as l} target-x]
  (let [ax (x-acceleration a p)
        [ok tl tr] (solve-square-equation (* 0.5 ax) vx (- x target-x))]
    (if-let [tta (and ok (if (<= 0.0 tl tr) tl (if (<= 0.0 tr) tr)))]
      [true (move lc tta l) tta]
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

(defn- approach-loop
  [^lander.Lander lander ^geometry.Section {ax :ax bx :bx :as section} ^lander.Control ctl]
  (loop [l lander t 0.0]
    (if (= ctl (:control l))
      [:ok l t]
      (let [l-next (move ctl 1.0 l)]
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
