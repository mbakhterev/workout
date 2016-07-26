(ns Player (:gen-class))

(defrecord Point [^double x ^double y])
(defrecord Section [^Point a ^Point b])

(defrecord Lander [^double x ^double y ^double dx ^double dy
                   ^long fuel ^long angle ^long power])

(defrecord Fitness [^double fitness path])

(defn- read-surface []
  (->> (apply list (repeatedly (* 2 (read)) read))
       (partition 2)
       (map (fn [p] (apply ->Point p)))
       (partition 2 1)
       (map (fn [c] (apply ->Section c)))))

(defn- read-lander [] (apply ->Lander (repeatedly 7 read)))

(defn- dump [& args] (binding [*out* *err*] (apply println args)))

(defn- ^boolean is-pad [^Segment s] (< -0.01 (- (-> c :a :y) (-> c :b :y)) 0.01))

; Берём первую попавшуюся посадочную площадку. Нам, кажется, гарантируют, что
; она одна такая

(defn- find-landing [S]
  (let [LR (group-by is-pad S)] [(first (LR true)) (LR false)]))

; Синусы и косинусы для рассчёта проекции тяги. Угол задаётся от оси (+ PI/2).
; Симметричность cos не учитываем, чтобы не усложнять формулу пересчёта угла phi в
; индекс таблицы i. Формула должна быть такой i = phi + 90

(def ^:const ^doubles cos-table
  (mapv (fn [d] (Math/cos (Math/toRadians (+ d 90)))) (range -90 91)))

(def ^:const ^doubles sin-table
  (mapv (fn [d] (Math/sin (Math/toRadians (+ d 90)))) (range -90 91)))

; Функции для доступа в таблицы по значению угла

(defn- ^double x-power [^long phi] (nth cos-table (+ phi 90)))
(defn- ^double y-power [^long phi] (nth sin-table (+ phi 90)))

; Функция оценки качества траектории

(defn- ^double fitness [^Lander l ^Section target ^double energy]
  (let [x   (:x l)
        y   (:y l)
        dx  (:dx l)
        dy  (:dy l)
        phi (:angle l)
        h   (-> target :a :y)
        ax  (-> target :a :x)
        bx  (-> target :b :x)]
    (+ (if (<= ax x bx) 0.0 (min (Math/abs (- x ax)) (Math/abs (-x bx))))
       (Math/abs (- y h))
       (Math/abs phi)
       (* 0.5 (- 40 dy) (- 40 dy))
       (* 0.5 (- 20 dx) (- 20 dy)))))

; Движение модуля l при управлении (vec angle power). Сохраняем новое положение
; модуля и то управление, которое привело его в это положение. Положение - это
; вектор в фазовом пространстве (vec x y dx dy fuel). Нужно быстро считать,
; поэтому juxt не используем.

(defn- ^Lander move [^Lander l ^long angle ^double power]
  (let [x    (:x l)
        y    (:y l)
        dx   (:dx l)
        dy   (:dy l)
        fuel (:fuel l)
        ddx  (* power (x-power angle))
        ddy  (* power (y-power angle))]
    (->Lander (+ x dx (* 0.5 ddx)) (+ y dy (* 0.5 ddy)) (+ dx ddx) (+ dy ddy)
              (- fuel power) angle power)))

; Экономим на копейках, на всякий случай рассчитываем все ходы заранее. Потому
; что в runtime придётся довольно много if-ов обрабатывать. А так, просто из
; таблицы выбор

(defn- gen-cloud [base cloud array-convert]
  (let [A (first base)
        B (last base)]
    (->> base
         (mapv (fn [p] (->> cloud
                            (map (partial + p))
                            (filter (fn [i] (<= A i B)))
                            array-convert
                            vec))))))

(def ^:const angle-step 5)
(def ^:const angle-cloud-table (gen-cloud (range -90 91)
                                          (range -15 (+ 15 angle-step) angle-step)
                                          long-array))

(defn- ^longs angle-cloud [^long phi] (angle-cloud-table (+ 90 phi)))

(def ^:const power-cloud (gen-cloud (range 0 5)
                                    (range -1.0 2.0)
                                    double-array))

; Можно придумать сценарии с различными полезными поведениями в разных режимах
; полёта. Не очевидно, как ограничивать возможные варианты управления. Поэтому
; рассматриваем всё с последующим обрезанием плохих траекторий

(defn- path-cloud [path]
  (let [l (first path)]
    (for [p (power-cloud (:power l)) a (angle-cloud (:angle l))] 
      (cons (move l a p) path))))

; Пересчёт ценностей путей в списке P

(defn- evaluate-paths [P ^Section target]
  (map (fn [p] (->Fitness (fitness (first p) target 0.0) p)) P))

(def ^:const ^long runtime-threshold (* 3))

(defn- lookup-path [^Lander l]
  (loop [paths (list (list l)) iterations 0]
    (cond (> iterations runtime-threshold)
          (do (dump "TIMEOUT. Paths generated:" (count paths)))

          :else
          (recur (mapcat path-cloud paths) (inc iterations)))))

(defn -main [& args]
  (let [S (read-surface)      
        G (read-lander)         
        [L R] (find-landing S)] 

      ; S - поверхность
      ; G - начальное состояние игры для анализа направления
      ; отрезки: L - места для посадки
      ;          R - опасные места, рядом с которыми не летаем

    (dump "power-cloud:" power-cloud)
    (dump "angle-cloud:" (take 5 (drop 5 angle-cloud-table)))
    (dump "surface:" S)
    (dump "game:" G)
    (dump "landings:" L)
    (dump "rocks:" R)
      
    (loop [game G] 
      (lookup-path game)

      (println 81 4)

      (let [next-game (read-lander)
            model     (move game (:angle next-game) (:power next-game))]
        (dump "model:" (map (comp (partial format "%.02f") double second) model))
        (dump "ngame:" (map (comp (partial format "%.02f") double second) next-game))

        (recur next-game)))))
