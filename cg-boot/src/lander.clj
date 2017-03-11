(ns lander (:require [render :as r]
                     [records :refer :all]))

(set! *warn-on-reflection* true)

; Вспомогательные функции
(defn- dump [& args] (binding [*out* *err*] (apply println args)))
(defn- read-surface [] (let [N (read)] (doall (repeatedly (* 2 N) read))))
(defn- read-lander [] (doall (repeatedly 7 read)))

; Тестовые данные
(def ^:private ^:const test-data [{:surface [0 1000 300 1500 350 1400 500 2000
                                             800 1800 1000 2500 1200 2100 1500 2400
                                             2000 1000 2200 500 2500 100 2900 800
                                             3000 500 3200 1000 3500 2000 3800 800
                                             4000 200 5000 200 5500 1500 6999 2800]
                                   :lander [500 2700 100 0 800 -90 0]}])

(defn- surface-points [raw-numbers]
  (map (fn [p] (apply ->Point p)) (partition 2 raw-numbers)))

(defn- surface-sections [points]
  (map (fn [s] (apply make-section s)) (partition 2 1 points)))

(defn- find-landing-pad [points]
  (letfn [(is-pad ([[a b]] (< -0.01 (- (:y a) (:y b)) 0.01)))]
    (apply make-section (first (filter is-pad (partition 2 1 points))))))

(defn- surface-shell [points landing]
  (letfn [(monotonize [points]
            (loop [[p & P] (rest points)
                   max-y (:y (first points))
                   R [(first points)]]
              (cond
                ; Обработка последней точки. Если она выше чем предыдущий
                ; максимальный уровень, то включаем её в список. Если нет, то
                ; накрываем поверхность горизонтальным сегментом на
                ; максимальном уровне
                (empty? P) (conj R (if (> (:y p) max-y) p (->Point (:x p) max-y)))
                
                ; Обновление максимума с добавлением точки в результат
                (> (:y p) max-y) (recur P (:y p) (conj R p))

                ; Пропускаем точку
                :else (recur P max-y R))))

          (sectionize [points]
            (map (fn [s] (apply make-section s)) (partition 2 1 points)))]
    (let [l-points (filter (fn [p] (<= (:x p) (:ax landing))) points)
          r-points (filter (fn [p] (>= (:x p) (:bx landing))) points)
          l-shell (reverse (monotonize (reverse l-points)))
          r-shell (monotonize r-points)]
      (comment (vec (concat (sectionize l-shell) (sectionize r-shell))))
      [(vec (sectionize l-shell)) (vec (sectionize r-shell))])))

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

(def ^:private ^:const dG 20.0)
(def ^:private ^:const dV 2.0)
(def ^:private ^:const nV 50) 
(def ^:private ^:const fV 38.0)  

(defn- grid-ceil [dG x] (* dG (Math/ceil (/ x dG))))
(defn- grid-floor [dG x] (* dG (Math/floor (/ x dG))))

(defn- build-row [dG nV l-side r-side height]
  (let [between (fn [a b h] (and (<= a h) (< h b)))
        xbyy (fn [y s] (+ (:ax s) (/ (- y (:ay s)) (:k s))))
        l (first (filter (fn [s] (between (:by s) (:ay s) height)) l-side))
        r (first (filter (fn [s] (between (:ay s) (:by s) height)) r-side))
        lx (grid-ceil dG (+ (if l (xbyy height l) 0.0) (* 2 dG)))
        rx (grid-floor dG (- (if r (xbyy height r) x-max) (* 2 dG)))
        n-cells (+ 1 (long (/ (- rx lx) dG))) ]
    (->Row lx (vec (repeatedly n-cells (fn [] (boolean-array (* 4 nV nV)))))))) 

(defn- build-grid [dG dV nV l-rock r-rock l-pad lander]
  ;         dG, dV: размеры ячеек по пространству и по скоростям
  ;             nV: количество шагов по скорости в каждом направлении: up, down,
  ;                 left, right
  ; l-rock, r-rock: левая и правая стороны скалы
  ;          l-pad: посадочная площадка
  ;         lander: начальная позиция модуля  
  (let [l (move-back (->Lander (:mx l-pad) (:ay l-pad) 0.0 -35.0 0 0 4 true) 0 4)
        h (- (:y l) (* 0.25 dG))
        top (min (+ (:y lander) (* 4 dG)) y-max)]
    (->Grid dG dV nV h (mapv (partial build-row dG nV l-rock r-rock) (range h top dG)))))



(defn- arrived? [dG dV T l]
  (and (<= (Math/abs ^double (- (:x T) (:x l))) dG)
       (<= (Math/abs ^double (- (:y T) (:y l))) dG)
       (<= (Math/abs ^double (- (:vx T) (:vx l))) dV)
       (<= (Math/abs ^double (- (:vy T) (:vy l))) dV)))

(defn- visit-speed [nV dV ^booleans array l]
  (let [lvx (:vx l)
        lvy (:vy l)
        offset (* nV nV (+ (if (neg? lvx) 2 0)
                           (if (neg? lvy) 1 0)))
        nx (Math/ceil (/ (Math/abs ^double lvx) dV))
        ny (Math/ceil (/ (Math/abs ^double lvy) dV))
        idx (+ offset (* ny nV) nx)]
    (if (and (< nx nV)
             (< ny nV)
             (not (aget array idx)))
      (do (aset array idx true)
          l))))

(defn- visit-grid [G l]
  (let [bl (:baseline G)
        R (:rows G)
        h (:y l)]
    (comment (println "bl R h (h ≥ bl)" bl (count R) h (>= h bl)))
    (if (>= h bl)
      (let [nr (Math/ceil (/ (- h bl) (:dG G)))]
        (if (< nr (count R))
          (let [x (:x l)
                r (nth R nr)
                C (:cells r)
                lx (:left r)]
            (if (>= x lx)
              (let [nc (Math/ceil (/ (- x lx) (:dG G)))]
                (if (< nc (count C))
                  (visit-speed (:nV G) (:dV G) (nth C nc) l))))))))))

(defn- landers-up [dG fV l-pad]
  (map (fn [x] (move-back (->Lander x (:ay l-pad) 0.0 (- fV) 0 0 0 true) 0 4))
       (range (+ (:ax l-pad) dG) (:bx l-pad) dG)))

(defn- estimate [T l]
  (let [dx (- (:x T) (:x l))
        dy (- (:y T) (:y l))
        dvx (- (:vx T) (:vx l))
        dvy (- (:vy T) (:vy l))]
    (+ (:fuel l) 
       (+ (* dvx dvx) (* dvy dvy)) 
       (Math/sqrt (+ (* dx dx) (* dy dy))))))

(defn- path-estimate [T p] (estimate T (first p)))

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
    (for [p (power-cloud (:power l)) a (angle-cloud (:angle l))] 
      (cons (move-back l a p) path))))

(defn- search [grid target paths]
  (let [cmp (fn [x y] (compare (path-estimate target x)
                               (path-estimate target y)))
        visit (fn [p] (visit-grid grid (first p)))]
    (loop [queue (apply sorted-set-cmp paths)]
      (if-let [p (first queue)]
        (if (arrived? (:dG grid) (:dV grid) target (first p))
          p
          (recur (into queue (doall (keep visit (path-cloud p))))))))))

(def ^:private ^:const test-id 0)
(def ^:private ^:const s-points (surface-points (:surface (test-data test-id))))
(def ^:private ^:const i-lander (apply ->Lander (conj (:lander (test-data test-id)) true))) 
(def ^:private ^:const l-pad (find-landing-pad s-points))
(def ^:private ^:const surface (surface-sections s-points))
(let [[l r] (surface-shell s-points l-pad)]
  (def ^:private ^:const shell (vec (concat l r)))
  (def ^:private ^:const l-shell l)
  (def ^:private ^:const r-shell r))

(def ^:private grid (build-grid dG dV nV l-shell r-shell l-pad i-lander))

(r/update-scene :surface surface)
(r/update-scene :landing-pad l-pad)
(r/update-scene :shell shell)

(r/update-scene :lander
                (concat (take-while (partial alive? shell)
                                    (reductions (wrap move) i-lander (repeat [0 2])))
                        (keep (partial visit-grid grid) (landers-up dG fV l-pad))))

(r/update-scene :grid grid)

(time (reduce + (map (comp count :cells) (:rows grid))))
