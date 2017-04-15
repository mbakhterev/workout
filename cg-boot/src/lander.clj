(ns lander (:require [geometry :refer :all]))

(set! *warn-on-reflection* true)

(comment (defn- debugln [flag & args]
  (let [flags (hash-set ; :hover-search
                        :search-path
                        ; :solve-hover
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

(defn- tune-value [current goal max-delta]
  (let [delta (- goal current)]
    (cond (= 0 delta) goal
          (< 0 delta) (if (< delta max-delta) goal (+ current max-delta))
          (> 0 delta) (if (< delta max-delta) goal (- current max-delta)))))

(defn- control-to [^Control f ^Control t]
  (->Control (tune-value (:angle f) (:angle t) angle-max-delta)
             (tune-value (:power f) (:power t) power-max-delta)))

(defn- x-acceleration [angle power] (* power (x-power angle)))
(defn- y-acceleration [angle power] (- (* power (y-power angle)) M))

(def ^:private ^:const power-cloud (range 0 5)) 
(def ^:private ^:const angle-delta 5)
(def ^:private ^:const angle-cloud (range -90 91 angle-delta))
(def ^:private ^:const control-cloud (for [a angle-cloud p power-cloud] (->Control a p)))

(defn move [^Control tc t ^Lander {lc :control :as l}] 
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

(def ^:private ^:const x-max (- 7000.0 1.0))
(def ^:private ^:const y-max (- 3000.0 1.0))

(defn- in-range? [{x :x} {ax :ax bx :bx}] (and (<= ax x) (< x bx)))

(defn- over-line? [{x :x y :y} {ax :ax ay :ay k :k}]
  (let [rx (- x ax)
        ry (- y ay)] (< (* k rx) ry)))

(defn over-section? [lander section] (and (in-range? lander section)
                                          (over-line? lander section)))

(defn alive? [surface {x :x y :y :as lander}]
  (and (<= 0 x x-max)
       (<= 0 y y-max)
       (over-line? lander (first (filter (partial in-range? lander) surface)))))

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

(defn- descend-constraint [{vy :vy}]
  (let [ve (- max-vy)
        ay (- 4.0 M)
        t  (/ (- ve vy) ay)]
    (if (< t 0.0)
      (->Constraint 0.0 0.0 0.0)
      (->Constraint 0.0 (+ (* vy t) (* 0.5 ay ay t)) t))))

(defn- brake-constraint [{vx :vx vy :vy} {left? :left?}]
  (let [ax (if left? -4.0 +4.0)
        t  (/ (- vx) ax)
        ay (- M)]
    (if (<= 0.0 t)
      (->Constraint (+ (* vx t) (* 0.5 ax ax t)) 
                    (+ (* vy t) (* 0.5 ay ay t))
                    t)))) 

(defn- constraint [{x :x h :y fuel :fuel :as l}
                   {xp :x-pad yp :y-pad left? :left? :as S}]
  (let [dc (descend-constraint l)
        bc (brake-constraint l S)
        x-cmp (if left? > <)]

    (debugln :constraint "dc:" dc "bc:" bc "dx:" (:dx bc))

    (and dc bc
         (x-cmp xp (+ x (reserve (:dx bc) dx-constraint-reserve)))
         (< yp (+ h (reserve (+ (:dh bc) (:dh dc)) h-constraint-reserve)))
         (< (* 4.0 (+ (:t bc) (:t dc))) fuel))))

(defn- add-braking-stage [{x :x vx :vx :as lander}
                          {ax :ax ay :ay bx :bx :as pad}
                          stages]
  (if (and (= 0 vx) (in-range? lander pad))
    stages
    (let [left? (or (< x ax) (< 0.0 vx))
          xp (if left? bx ax)]
      (cons (->Stage :braking left? pad pad xp xp ay nil) stages))))

(defn- add-hover-stages [{x :x}
                         {ax :ax ay :ay bx :bx :as pad}
                         l-rock r-rock stages] 
  (let [on-left (fn [s] (if (< x (:bx s) bx) (->Stage :hover true s pad (:bx s) bx ay nil)))
        on-right (fn [s] (if (> x (:ax s) ax) (->Stage :hover false s pad (:ax s) ax ay nil)))]
    (concat (cond (< x ax) (keep on-left l-rock)
                  (> x bx) (keep on-right (reverse r-rock)))
            stages)))

(defn- add-reverse-stage [{x :x vx :vx :as lander}
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

(defn- add-descend-stage [{x :x} {ax :ax ay :ay bx :bx :as pad}]
  (let [left? (< x ax)
        xp (if left? bx ax)]
    (list (->Stage :descending left? pad pad xp xp ay nil))))

(defn detect-stages [l l-rock pad r-rock]
  (->> (add-descend-stage l pad)
       (add-braking-stage l pad)
       (add-hover-stages l pad l-rock r-rock)
       (add-reverse-stage l pad l-rock r-rock)))

(defn solve-square-equation [a b c]
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
  (debugln :solve-hover l x vx x-target (- x x-target))
  (let [ax (x-acceleration a p)
        r  (solve-square-equation (* 0.5 ax) vx (- x x-target))]
    (debugln :solve-hover ax r)
    (if r 
      (let [{tl :left tr :right} r
            tta (if (<= 0.0 tl) tl (if (<= 0.0 tr) tr))]
        (->Move :ok (move lc tta l) tta)))))

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

(defn align-control [lander {section :section} ctl]
  (loop [l lander t 0.0]
    (cond (not (over-line? l section)) nil ; FIXME: Теряем некоторые траектории  
          (not (in-range? l section))  (->Move :out l t)   
          (= ctl (:control l))         (->Move :ok l t)
          :else                        (recur (move ctl 1.0 l) (+ 1.0 t)))))

(defn- trace-hover [traces {section :section :as stage} {ctl :control :as lander} t]
  (debugln :trace-hover lander t stage)
  (if (traces ctl)
    traces
    (let [solution (solve-hover lander stage)]
      (if-not solution
        (assoc traces ctl (->Move :ko nil 0.0))
        (let [{tta :dt l :lander} solution]
          (debugln :trace-hover l (constraint l stage) "solution:" solution)
          (if-not (and (constraint l stage)
                       (over-line? l section))
            (assoc traces ctl (->Move :ko nil 0.0))
            (let [t-ceil    (Math/ceil tta)
                  t-overall (+ t t-ceil)]
              (assoc traces ctl (->Move :ok (move ctl t-ceil lander) t-overall))))))))) 

(defn integrate-hover [stage lander traces ctl]
  (debugln :integrate-hover stage lander ctl)
  (let [{state :state l :lander t :dt :as m} (align-control lander stage ctl)]
    (debugln :integrate-hover state l t)
    (case state
      nil  traces
      :ok  (trace-hover traces stage l t)
      :out (let [c (:control l)] (if (traces c) traces (assoc traces c m))))))

(defn get-landers [timed? traces]
  (let [keepfn (fn [{st :state l :lander t :dt}] (if (not= st :ko) (if timed? [l t] l)))
        keyfn  (if timed? (comp - :fuel first) (comp - :fuel))]
    (sort-by keyfn (keep keepfn (vals traces)))))

(declare search-path)

(defn- hover-search [stage next-stages ^Lander lander depth]
;  (debugln :hover-search depth lander)
  (debugln :hover-search "HS" depth lander)
  (let [traces (reduce (partial integrate-hover stage lander) {} control-cloud)]
    (debugln :hover-search "HS traced" depth (count traces))
    (loop [L (get-landers false traces)]
      (debugln :hover-search depth (count L))
      (if-not (empty? L)
        (let [{ctl :control :as l} (first L)
              ctl-next (search-path next-stages l (+ 1 depth))]
;          (debugln :hover-search ctl-next)
          (if ctl-next 
            (cons ctl ctl-next)
            (recur (next L))))))))

(defn search-path [stages ^Lander lander depth]
  (debugln :search-path "SP" depth)
  (if-let [s (first stages)]
    (case (:stage s)
      :hover (hover-search s (next stages) lander depth)
      (list))))

(defn model-hover [^Control control ^Stage stage ^Lander lander]
  (let [x-goal (:x-goal stage)
        ctl-move (partial move control 1.0)
        on-stage? (if (:left? stage)
                    (fn [l] (<= (:x l) x-goal))
                    (fn [l] (>= (:x l) x-goal)))]
    (loop [l (ctl-move lander) L [lander]]
      (if (on-stage? l)
        (recur (ctl-move l) (conj L l))
        (conj L l)))))

(defn model-control [controls stages ^Lander lander]
  (->> (map list stages controls)
       (filter (fn [p] (= :hover (:stage (first p)))))
       (reductions (fn [trace [s c]] (model-hover c s (last trace))) [lander])))
