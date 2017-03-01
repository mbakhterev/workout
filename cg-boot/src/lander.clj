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

(def ^:private ^:const ^double M 3.711)

(defn- move [l [angle power]]
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

(defn- move-back [l [angle power]]
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

(defn- to-cell-x [x] (+ rG (* dG (Math/floor (/ x dG)))))

(defn- build-row [l-side r-side height]
  (let [between (fn [a b h] (and (<= a h) (< h b)))
        xbyy (fn [y s] (+ (:ax s) (/ (- y (:ay s)) (:k s))))
        cell-bottom (- height rG)
        l (first (filter (fn [s] (between (:by s) (:ay s) cell-bottom)) l-side))
        r (first (filter (fn [s] (between (:ay s) (:by s) cell-bottom)) r-side))
        lx (to-cell-x (+ (if l (xbyy cell-bottom l) 0.0) dG))
        rx (to-cell-x (- (if r (xbyy cell-bottom r) x-max) dG))]
    [(->Point lx height)
     (->Point rx height)])) 

(defn- build-grid [l-shell r-shell l-pad]
  (let [l (move-back (->Lander (:mx l-pad) (:ay l-pad) 0.0 -40.0 10 0 4 true) [0 4])
        h (:y l)]
    [(conj (build-row l-shell r-shell h) (->Point (:x l) h))]))

(def ^:private ^:const test-id 0)
(def ^:private s-points (surface-points (:surface (test-data test-id))))
(def ^:private i-lander (apply ->Lander (conj (:lander (test-data test-id)) true))) 
(def ^:private l-pad (find-landing-pad s-points))
(def ^:private surface (surface-sections s-points))
(let [[l r] (surface-shell s-points l-pad)]
  (def ^:private shell (vec (concat l r)))
  (def ^:private l-shell l)
  (def ^:private r-shell r))

(r/update-scene :surface surface)
(r/update-scene :landing-pad l-pad)
(r/update-scene :shell shell)

(r/update-scene :lander
                (take-while (partial alive? shell)
                            (reductions move i-lander (repeat [0 2]))))

(r/update-scene :grid (build-grid l-shell r-shell l-pad))
