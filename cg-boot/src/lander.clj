(ns lander (:require [geometry :as g]))

(set! *warn-on-reflection* true)

(defn debugln [flag & args]
  (let [flags (hash-set ; :hover-search
                        ;  :search-guide
                        ; :solve-hover
                        ; :brake-integrate
                        ; :solve-brake-4
                        ; :hover-guide
                        ; :hover-integrate
                        ; :solve-descend-one
                        ; :along-guide
                        ; :make-guide
                        )]
    (if (flags flag) (binding [*out* *err*] (apply println args)))))

(defrecord Control [^long angle ^long power])

(defrecord Lander [^double x
                   ^double y
                   ^double vx
                   ^double vy
                   ^long fuel
                   ^Control control])

(defn form-lander [nums]
  (let [[m c] (split-at 5 nums)]
    (apply ->Lander (conj (vec m) (apply ->Control c))))) 

(defrecord Constraint [^double x ^double h ^double t])

; (defrecord Roots [^double left ^double right])

(defrecord Move [state ^Lander lander ^double dt])

(let [angle-max-delta 15
      power-max-delta 1
      tune-value (fn [^long current ^long goal ^long max-delta]
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
      to-zero (fn [^double a] (if (g/non-zero? a) a 0.0))

      make-table (fn [f] (vec (for [p (range 0 5)]
                                (vec (for [a (range -90 91)]
                                       (to-zero (f a p)))))))

      x-table (make-table x-force)
      y-table (make-table y-force)]
  (defn- x-acceleration [^long a ^long p] ((x-table p) (+ 90 a)))
  (defn- y-acceleration [^long a ^long p] ((y-table p) (+ 90 a))))

; Движение модуля l при управлении (vec angle power). Сохраняем новое положение
; модуля и то управление, которое привело его в это положение. Положение -
; вектор в фазовом пространстве (vec x y dx dy fuel)
 
(defn move [^Control ctl
            ^double t
            ^Lander {lc :control :as l}] 
  (let [{angle :angle power :power :as nc} (control-to lc ctl)
        ax (x-acceleration angle power)
        ay (y-acceleration angle power)
        {x :x y :y vx :vx vy :vy fuel :fuel} l]
    (assert (or (= 1.0 t) (= nc ctl)))
    (->Lander (+ x (* vx t) (* 0.5 ax t t))
              (+ y (* vy t) (* 0.5 ay t t))
              (+ vx (* ax t))
              (+ vy (* ay t))
              (- fuel (* power t))
              nc)))

; Проверки того, что модуль над поверхностью Марса

(defn- in-range? [^Lander {x :x} ^geometry.Section s] (g/in-range? x s))

(defn- over-line? [^Lander {x :x y :y} ^geometry.Section s] (g/over-line? s x y))

(defn- over-section? [^Lander lander ^geometry.Section section]
  (and (in-range? lander section)
       (over-line? lander section)))

(def ^:const ^:private x-max (- 7000.0 1.0))
(def ^:const ^:private y-max (- 3000.0 1.0))

(defn- on-radar? [^Lander {x :x y :y}] (and (<= 0 x x-max) (<= 0 y y-max)))

(defn alive? [^geometry.Section surface ^Lander {x :x y :y :as lander}]
  (and (on-radar? lander)
       (over-line? lander (first (filter (partial in-range? lander) surface)))))

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

(defn- descend-constraint [^Lander {vy :vy}]
  (let [ve (- max-final-vy)
        ay (y-acceleration 0 4)
        t  (/ (- ve vy) ay)]
    (if (< t 0.0)
      (->Constraint 0.0 0.0 0.0)
      (->Constraint 0.0 (+ (* vy t) (* 0.5 ay ay t)) t))))

(comment (defn- brake-constraint [^Lander {vx :vx vy :vy} ^geometry.Stage {dir :direction}]
           (let [xi (if left? 90 -90)
                 ay (y-acceleration xi 4)
                 ax (x-acceleration xi 4)
                 t  (/ (- vx) ax)]
             (if (<= 0.0 t)
               (->Constraint (+ (* vx t) (* 0.5 ax ax t)) 
                             (+ (* vy t) (* 0.5 ay ay t))
                             t))))) 

(defn- brake-constraint [^Lander {vx :vx vy :vy} ^long direction]
  (let [xi (* direction 90)
        ay (y-acceleration xi 4)
        ax (x-acceleration xi 4)
        t  (/ (- vx) ax)]
    (if (<= 0.0 t)
      (->Constraint (+ (* vx t) (* 0.5 ax ax t)) 
                    (+ (* vy t) (* 0.5 ay ay t))
                    t))))

(comment (defn- constraint [^Lander {x :x h :y fuel :fuel :as l}
                            ^geometry.Stage {xp :x-pad yp :y-pad left? :left? :as S}]
           (if-let [bc (brake-constraint l S)]
             (let [dh-reserve 0.125
                   dx-reserve 0.125
                   dc (descend-constraint l)
                   x-cmp (if left? < >)
                   xr (+ x (reserve (:x bc) dx-reserve))
                   hr (+ h (reserve (+ (:h bc) (:h dc)) dh-reserve))]
               (and (x-cmp xr xp)
                    (< yp hr)
                    (< (* 4.0 (+ (:t bc) (:t dc))) fuel))))))

(defn- constraint [^Lander {x :x h :y fuel :fuel :as l}
                   ^geometry.Stage {xp :x-pad yp :y-pad dir :direction}]
  (if-let [bc (brake-constraint l dir)]
    (let [dc (descend-constraint l)
          xr (+ x (reserve-dx (:x bc)))
          hr (+ h (reserve-dh (+ (:h bc) (:h dc))))]
      (and (neg? (* dir (- xr xp)))
           (< yp hr)
           (< (* 4.0 (+ (:t bc) (:t dc))) fuel)))))

; (defn- brake-stage [^Lander {x :x vx :vx :as lander}
;                     ^geometry.Section {ax :ax ay :ay bx :bx :as pad}
;                     stages]
;   (if (and (= 0 vx) (in-range? lander pad))
;     stages
;     (let [left? (or (< x ax) (< 0.0 vx))
;           xp (if left? bx ax)]
;       (cons (->Stage :brake left? pad pad xp xp ay nil) stages))))
; 
; (defn- hover-stages [^Lander {x :x :as l}
;                      ^geometry.Section {ax :ax ay :ay bx :bx :as pad}
;                      l-rock r-rock stages]
;   (letfn [(on-left [^Stage s]
;             (if (< x (:bx s) bx)
;               (map (fn [^double t] (->Stage :hover true s pad t bx ay nil))
;                    (divide-stage (max x (:ax s)) (:bx s)))))
;           (on-right [^Stage s]
;             (if (> x (:ax s) ax)
;               (map (fn  [^double t] (->Stage :hover false s pad t ax ay nil))
;                    (divide-stage  (min x (:bx s)) (:ax s)))))
;           (divide-stage [^double a ^double t]
;             (if (< (Math/abs (- t a)) 2048.0)
;               (list t)
;               (let [m (+ a (/ (- t a) 2.0))] (concat (divide-stage a m) (divide-stage m t)))))]
;     (concat (cond (< x ax) (mapcat on-left l-rock)
;                   (> x bx) (mapcat on-right (reverse r-rock)))
;       stages)))
; 
; (defn- reverse-stage [^Lander {x :x vx :vx :as lander}
;                       ^geometry.Section {ax :ax ay :ay bx :bx :as pad}
;                       l-rock r-rock stages]
;   (if-not (or (and (< x ax) (< vx 0.0))
;               (and (> x bx) (> vx 0.0)))
;     stages
;     (let [left? (< x ax)
;           rock (if left? l-rock r-rock)
;           s (first (filter (partial in-range? lander) rock))
;           surface (if left?
;                     (take-while (fn [r] (<= (:ax r) x)) rock)
;                     (drop-while (fn [r] (<= (:bx r) x)) rock))]
;       (cons (->Stage :reverse left? s pad (if left? (:bx s) (:ax s)) (if left? bx ax) ay surface)
;             stages))))
; 
; (defn- descend-stage [^Lander {x :x}
;                       ^geometry.Section {ax :ax ay :ay bx :bx :as pad}]
;   (let [left? (< x ax)
;         xp (if left? bx ax)]
;     (list (->Stage :descend left? pad pad xp xp ay nil))))
; 
; (defn detect-stages [l ^geometry.Landscape {l-rock :left-rock r-rock :right-rock pad :landing-pad}]
;   (->> (descend-stage l pad)
;        (brake-stage l pad)
;        (hover-stages l pad l-rock r-rock)
;        (reverse-stage l pad l-rock r-rock)))

(declare hover-guide
         brake-guide
         descend-guide)

(defn search-guide [stages ^Lander lander]
  (if-let [s (first stages)]
    (case (:stage s)
      :brake (do (debugln :search-guide "brake") (brake-guide s (rest stages) lander))
      :hover (do (debugln :search-guide "hover")(hover-guide s (rest stages) lander))
      :descend (do (debugln :search-guide "brake") (descend-guide s lander))
      (list))))

; (defn- near-zero? [^double a] (> 1E-10 (Math/abs a)))

; (defn- solve-square-equation [^double a ^double b ^double c]
;   (if (near-zero? a) 
;     (if (not= 0.0 b)
;       (let [t (/ (- c) b)] (->Roots t t)))
;     (let [D (- (* b b) (* 4.0 a c))]
;       (if (<= 0.0 D)
;         (let [D-sqrt (Math/sqrt D)
;               a-rcpr (/ (* 2.0 a))
;               tp     (* (+ (- b) D-sqrt) a-rcpr)
;               tm     (* (- (- b) D-sqrt) a-rcpr)]
;           (->Roots (min tp tm) (max tp tm)))))))

; Необходимо найти экстремальную точку на отрезке [0; t]

; (defn- square-extremum [^double a ^double b ^double c ^double t]
;   (if (near-zero? a)
;     (+ (* b t) c)
;     (let [tx (/ (- b) a 2.0)]
;       (if (<= 0.0 tx t)
;         (+ (* a tx tx) (* b tx) c)
;         (if (< t tx)
;           (+ (* a t t) (* b t) c)
;           c)))))

; (defn- solve-hover [^Lander {x :x vx :vx {a :angle p :power :as ctl} :control :as l}
;                     ^Stage {x-target :x-goal}]
;   (let [ax (x-acceleration a p)
;         r  (solve-square-equation (* 0.5 ax) vx (- x x-target))]
;     (if r 
;       (let [{tl :left tr :right} r
;             tm (Math/ceil (if (<= 0.0 tl) tl tr))]
;         (if (<= 0.0 tm) 
;           (->Move :ok (move ctl tm l) tm))))))

(comment (defn- solve-hover [^Lander {x :x vx :vx {a :angle p :power :as ctl} :control :as l}
                             ^Stage {x-target :x-target}]
           (let [ax (x-acceleration a p)
                 r  (g/positive-root-of-square-equation (* 0.5 ax) vx (- x x-target))]
             (if-not (Double/isNaN r) 
               (let [tm (Math/ceil r)]
                 (->Move :ok (move ctl tm l) tm))))))

(defn- solve-hover [^Lander {x :x vx :vx {a :angle p :power :as ctl} :control :as l}
                    ^Stage {x-target :x-target}]
  (let [ax (x-acceleration a p)
        r  (g/time-to-intersect-x ax vx x x-target)]
    (if-not (Double/isNaN r) 
      (let [tm (Math/ceil r)]
        (->Move :ok (move ctl tm l) tm)))))

(defn- hover-align-control [^Stage {section :section :as stage} ^Lander lander ^Control ctl]
  (loop [l lander t 0.0]
    (cond (not (on-radar? l))          (->Move :ko l 0.0)
          (not (over-line? l section)) (->Move :ko l 0.0)
          (not (in-range? l section))  (if (constraint l stage) (->Move :out l t)  (->Move :ko l 0.0))
          (= ctl (:control l))         (->Move :ok l t)
          :else                        (recur (move ctl 1.0 l) (+ 1.0 t)))))

(defn- hover-integrate-ok-one [^Stage {section :section :as stage}
                               ^Lander {y :y vy :vy {a :angle p :power} :control :as lander}]
  (if-let [m (solve-hover lander stage)]
    (let [{l :lander t :dt} m]
      (if (and (constraint l stage)
               (on-radar? l)
               (over-line? l section)
               (> (- y-max 10) (g/square-extremum (* 0.5 (y-acceleration a p)) vy y t)))
        m))))

(defn- hover-integrate [^geometry.Stage stage
                       ^Lander lander
                       ^Control ctl]
  (let [{state :state :as ma} (hover-align-control stage lander ctl)]
    (case state
      :ko nil
      :ok (if-let [mi (hover-integrate-ok-one stage (:lander ma))] (list mi ma))
      :out (list ma))))

; Облако возможных управлений на стадии hover

(def ^:const ^:private angle-delta 15) 

(defn- hover-control-cloud [^geometry.Stage stage ^Lander lander]
  (for [p (range 0 5)
        a (let [a-left (:angle (:control (:lander (hover-align-control stage lander (->Control -90 p)))))
                a-right (:angle (:control (:lander (hover-align-control stage lander (->Control 90 p)))))]
            (if-not (:left? stage)
              (range a-left (+ a-right 1) angle-delta)
              (range a-right (- a-left 1) (- angle-delta))))]
    (->Control a p)))

(defn- hover-guide [^geometry.Stage stage next-stages ^Lander lander]
  (loop [cloud (keep (partial hover-integrate stage lander) (hover-control-cloud stage lander))]
    (when-first [m cloud]
      (if-let [m-next (search-guide next-stages (:lander (first m)))]
        (cons m m-next)
        (recur (next cloud))))))

(defn- brake-align-control [^Lander lander ^geometry.Stage {pad :pad} ^Control ctl]
  (loop [l lander t 0.0]
    (cond (not (on-radar? l))         nil
          (not (over-section? l pad)) nil
          (= ctl (:control l))        (->Move :ok l t)
          :else                       (recur (move ctl 1.0 l) (+ 1.0 t)))))

; Торможение - это 4 стадии: (1) торможение с переходом к выбранному контролю;
; (2) торможение с выбранным контролем; (3) торможение во время перехода к
; (контроль 0 4); Поэтому несколько движений. Решение на стадии (2) можно
; принимать лишь после моделирования стадии 3. Поэтому порядок таков. Кажется,
; движение аддитивно, поэтому можно обойтись без дополнительных повторных
; рассчётов.  Вроде как, считать не долго, поэтому ограничения на высоту
; проверяем в самом конце.

; Решение для стадии (2) торможения. Тонкости. (2.1) считаем, что целевая vx
; равна 0. (2.2) считаем, что должны хотя бы 0 секунд тормозить. Иначе, нам дали
; плохой контроль, и можно было бы потратить меньше топлива на остановку.

(defn- solve-brake-2 [^Lander {vx :vx {angle :angle power :power :as ctl} :control :as lander}
                      ^geomtrey.Stage {pad :pad :as stage}]
  (let [ax (x-acceleration angle power)]
    (if (= ax 0.0)
      (->Move :ok lander 0.0)
      (let [tb (/ (- vx) ax)]
        (if (<= 0.0 tb)
          (let [t (Math/ceil tb)
                l (move ctl t lander)]
            (if (and (over-section? l pad)
                     (on-radar? l))
              (->Move :ok l t))))))))

(defn- brake-integrate [^geometry.Stage {pad :pad :as stage}
                        ^Lander lander
                        ^Control ctl]
  (when-let [m-1 (brake-align-control lander stage ctl)]
    (when-let [m-3 (brake-align-control (:lander m-1) stage (->Control 0 4))]
      (when-let [m-2 (solve-brake-2 (assoc (:lander m-3) :control ctl) stage)]
        (when-let [dc (descend-constraint (:lander m-2))]
          (let [dh-reserve 0.125
                l (:lander m-2)
                hr (+ (:y l) (reserve (:h dc) dh-reserve))]
          (if (and (< (:ay pad) hr)
                   (< (* 4.0 (:t dc)) (:fuel (:lander m-3))))
            (list m-3 m-2 m-1)))))))) 

; По скорости можно определить какой диапазон ускорений следует рассматривать

(defn- brake-control-cloud [^Lander {vx :vx}]
  (if (>= vx 0.0)
    (for [p (range 1 5) a (range 0 91 angle-delta)] (->Control a p))
    (for [p (range 1 5) a (range -90 1 angle-delta)] (->Control a p))))

(defn- brake-guide [^geometry.Stage stage next-stages ^Lander lander]
  (loop [cloud (keep (partial brake-integrate stage lander) (brake-control-cloud lander))]
    (when-first [m cloud]
      (let [k (:lander (first m))
            l (:lander (second m))]
        (if-let [m-next (search-guide next-stages (assoc l :control (:control k)))]
        (cons m m-next)
        (recur (next cloud)))))))

; Решение для стадии (4) торможения. Тонкости. (4.1) ищем такой угол a для
; (контроль a 4), который позволит погасить остаточную скорость, оставаясь в
; границах pad.

(defn solve-descend-one [^Lander {x :x vx :vx y :y :as lander}
                          ^geometry.Stage {pad :pad :as stage}]
  (if (= 0.0 vx)
    (->Move :done lander 0.0)
    (let [vy-average -20.0
          bx (if (< 0.0 vx) (:bx pad) (:ax pad)) 
          tb (/ (- bx x) vx) 
          ta (Math/ceil (/ (- (:ay pad) y) vy-average))]
      (if (and (< (Math/abs ^double vx) max-final-vx)
               (<= ta tb))
        (->Move :done lander 0.0)
        (let [xi (if (> 0.0 vx) 7 -7)
              ax (x-acceleration xi 4)
              tc (Math/ceil (/ (- vx) ax))
              l  (move (->Control xi 4) tc lander)
              dc (descend-constraint l)]
          (if (and (in-range? l pad)
                   (< (:ay dc) (+ (:y l) (:h dc))))
            (-> Move :ok l tc))))))) 

(defn descend-guide [^geometry.Stage stage ^Lander lander]
  (loop [l lander R (list)]
    (if-let [m (solve-descend-one l stage)]
      (case (:state m)
        :done R
        :ok (recur (:lander m) (cons m R))))))

(defn model-control [guide ^Lander lander]
  (letfn [(do-control [moves ^Lander l]
            (when-first [{{ctl :control} :lander t :dt} moves]
              (let [landers (take (+ 1 t) (iterate (partial move ctl 1.0) l))]
                (cons landers
                      (do-control (next moves) (last landers))))))]
    (do-control (mapcat reverse guide) lander)))

(defn- along-guide-cloud [^Lander {{angle :angle power :power} :control :as lander}]
  (for [p (range (max 0 (- power 1)) (min (+ power 1 1) 5))
        a (range (max -90 (- angle 15)) (min (+ angle 15 1) 91))]
    (move (->Control a p) 1.0 lander)))

(defn- diff-landers [^Lander a ^Lander b]
  (let [dx (- (:x a) (:x b))
        dy (- (:y a) (:y b))
        dvx (- (:vx a) (:vx b))
        dvy (- (:vy a) (:vy b))]
    (+ (* dx dx)
       (* dy dy)
       (* dvx dvx)
       (* dvy dvy)))) 

(defn along-guide [^Lander lander guide]
  (if (not (empty? guide))
    (let [ig (first (reduce-kv (fn [[^long k ^double M :as r] ^long i ^Lander g]
                                 (let [mi (diff-landers lander g)]
                                   (if (<= M mi) r [i mi])))
                               [0 (diff-landers lander (nth guide 0))]
                               guide))]
      (debugln :along-guide "ig:" ig)
      (if (> (- (count guide) 1) ig)
        (let [target (nth guide (+ 1 ig))
              cloud (along-guide-cloud lander)
              [delta closest] (apply min-key first (map (juxt (partial diff-landers target) identity) cloud))]
          [delta (:control closest)])))))
