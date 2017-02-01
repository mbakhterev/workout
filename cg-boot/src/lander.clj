(ns lander (:require [render :as r]
                     [records :refer :all]))

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
  (letfn [(^boolean is-pad ([[^records.Point a ^records.Point b]]
                            (< -0.01 (- (:y a) (:y b)) 0.01)))]
    (apply make-section (first (filter is-pad (partition 2 1 points))))))

(defn- surface-shell [points ^records.Section landing]
  (letfn [(monotonize [points]
            (loop [[^records.Point p & P] (rest points)
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
    (let [l-points (filter (fn [^records.Point p] (<= (:x p) (:ax landing))) points)
          r-points (filter (fn [^records.Point p] (>= (:x p) (:bx landing))) points)
          l-shell (reverse (monotonize (reverse l-points)))
          r-shell (monotonize r-points)]
      (vec (concat (sectionize l-shell) (sectionize r-shell))))))

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

(defn- ^records.Lander move [^records.Lander l [^long angle ^double power]]
  (let [x    (:x l)
        y    (:y l)
        vx   (:vx l)
        vy   (:vy l)
        fuel (:fuel l)
        ax  (* power (x-power angle))
        ay  (- (* power (y-power angle)) M)]
    (->Lander (+ x vx (* 0.5 ax)) (+ y vy (* 0.5 ay)) (+ vx ax) (+ vy ay)
              (- fuel power) angle power)))

(defn- ^boolean is-alive [surface ^records.Lander l]
  (let [x (:x l)
        y (:y l)]
    (and (<= 0 x 6999)
         (<= 0 y 2999)
         (not (some (fn [s] (and (< (:ax s) x (:bx s))
                                 (let [rx (- x (:ax s))
                                       ry (- y (:ay s))]
                                   (<= ry (* (:k s) rx)))))
                    surface)))))

(def ^:private s-points (surface-points (:surface (test-data 0))))
(def ^:private i-lander (apply ->Lander (:lander (test-data 0)))) 
(def ^:private l-pad (find-landing-pad s-points))
(def ^:private surface (surface-sections s-points))
(def ^:private shell (surface-shell s-points l-pad))

(r/update-scene :surface surface)
(r/update-scene :landing-pad l-pad)
(r/update-scene :shell shell)
(r/update-scene :lander (take-while (partial is-alive shell)
                                    (reductions move i-lander (repeat [0 4]))))
