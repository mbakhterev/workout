(ns Player (:gen-class))

(defrecord Point [^double x ^double y])

; mx - это середина отрезка по оси x. Может оказаться полезной для fitness.
; Считаем заранее

(defrecord Section [^double ax ^double ay ^double bx ^double by
                    ^double k ^double mx])

(defn- make-section [^Point a ^Point b]
  (Section. (:x a) (:y a) (:x b) (:y b)
            (double (/ (- (:y b) (:y a))
                       (- (:x b) (:x a))))
            (+ (:x a)
               (/ (- (:x b) (:x a)) 2.0))))

(defrecord Lander [^double x ^double y ^double dx ^double dy
                   ^long fuel ^long angle ^long power])

(defrecord Fitness [^double fitness path])

(defn- read-lander [] (apply ->Lander (repeatedly 7 read)))

(defn- dump [& args] (binding [*out* *err*] (apply println args)))

; Синусы и косинусы для рассчёта проекции тяги. Угол задаётся от оси (+ PI/2).
; Симметричность cos не учитываем, чтобы не усложнять формулу пересчёта угла phi
; в индекс таблицы i. Формула должна быть такой i = phi + 90

(def ^:const ^doubles cos-table
  (mapv (fn [d] (Math/cos (Math/toRadians (+ d 90)))) (range -90 91)))

(def ^:const ^doubles sin-table
  (mapv (fn [d] (Math/sin (Math/toRadians (+ d 90)))) (range -90 91)))

; Функции для доступа в таблицы по значению угла

(defn- ^double x-power [^long phi] (nth cos-table (+ phi 90)))
(defn- ^double y-power [^long phi] (nth sin-table (+ phi 90)))

; Движение модуля l при управлении (vec angle power). Сохраняем новое положение
; модуля и то управление, которое привело его в это положение. Положение - это
; вектор в фазовом пространстве (vec x y dx dy fuel). Нужно быстро считать,
; поэтому juxt не используем.

(def ^:const ^double M 3.711)

(defn- ^Lander move [^Lander l ^long angle ^double power]
  (let [x    (:x l)
        y    (:y l)
        dx   (:dx l)
        dy   (:dy l)
        fuel (:fuel l)
        ddx  (* power (x-power angle))
        ddy  (- (* power (y-power angle)) M)]
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

; Функция оценки качества траектории

(defn- ^double fitness [^Lander l ^Section target ^double energy]
  (let [x    (:x l)
        y    (:y l)
        adx  (Math/abs (:dx l))
        ady  (Math/abs (:dy l))
        phi  (:angle l)
        fuel (:fuel l)
        h    (-> target :ay)
        ax   (-> target :ax)
        bx   (-> target :bx)]
    (+ (/ (if (<= ax x bx) 0.0 (min (Math/abs (- x ax)) (Math/abs (- x bx)))) 3000)
       (/ (Math/abs (- y h)) 7000)
       (/ (Math/abs phi) 90)
       (/ (if (<= ady 40.0) 0.0 (- ady 40.0)) 40)
       (/ (if (<= adx 20.0) 0.0 (- adx 20.0)) 20)
       (if (<= fuel 0) 1.0 (/ 1.0 fuel)))))

; Пересчёт ценностей путей в списке P. Возвращаем отсортированный список

(defn- evaluate-paths [P ^Section target]
  (sort-by :fitness (map (fn [p] (->Fitness (fitness (first p) target 0.0) p)) P)))

; Слияние отсортированных по fitness последовательностей. Чем fitness меньше,
; тем лучше

(defn- merge-fitness [paths-p paths-q]
  (loop [P paths-p
         Q paths-q
         R (transient [])]
    (let [p (first P)
          q (first Q)]
      (cond (nil? p) (concat (persistent! R) Q)
            (nil? q) (concat (persistent! R) P)

            (< (:fitness p) (:fitness q)) (recur (rest P) Q (conj! R p))
            :else                         (recur P (rest Q) (conj! R q))))))

(def ^:const ^long runtime-threshold (* 128))

(defn- lookup-path [^Lander l ^Section target]
  (loop [P (evaluate-paths (list (list l)) target)
         its 0]
    (cond (> its runtime-threshold)
            (do (dump "TIMEOUT. Paths generated:" (count P)
                      "max depth:" (apply max (map (comp count :path) P)))
                (reverse (:path (first P))))

          :else
            (recur (-> (path-cloud (:path (first P)))
                       (evaluate-paths target)
                       (merge-fitness (rest P))) (inc its)))))

(defn- ^boolean not-aligned [^Lander l ^Lander m]
  (let [c (juxt :x :y :dx :dy)]
    (> (Math/sqrt (reduce + (map (comp (fn [x] (* x x)) -) (c l) (c m)))) 2.0)))

(defn- dump-lander [^String desc ^Lander l]
  (dump desc \tab (map (comp (partial format "%.02f") double second) l)))

(defn- read-surface-points []
  (->> (apply list (repeatedly (* 2 (read)) read))
       (partition 2)
       (map (fn [p] (apply ->Point p)))))

(defn- read-surface []
  (->> (read-surface-points)
       (partition 2 1)
       (map (fn [c] (apply make-section c)))))

; Берём первую попавшуюся посадочную площадку. Нам, кажется, гарантируют, что
; она одна такая

(defn- reconstruct-surface [points]
  (letfn [(^boolean is-pad [[^Point a ^Point b]]
            (< -0.01 (- (:y a) (:y b)) 0.01))

          (find-target [points] (first (filter is-pad (partition 2 1 points))))

          (monotonize [points]
            (loop [[p & P] (rest points)
                   max-y (:y (first points))
                   R [(first points)]]
              (cond (empty? P)
                    (conj R (if (> (:y p) max-y) p (->Point (:x p) max-y)))

                    (> (:y p) max-y) (recur P (:y p) (conj R p))
                    :else (recur P max-y R))))
          
          (sectionize [points]
            (map (fn [c] (apply make-section c)) (partition 2 1 points)))]

    (let [[target-a target-b] (find-target points)
          points-l (filter (fn [p] (<= (:x p) (:x target-a))) points)
          points-r (filter (fn [p] (>= (:x p) (:x target-b))) points)
          mono-l (reverse (monotonize (reverse points-l)))
          mono-r (monotonize points-r)]

      (dump "target a:" target-a "target-b:" target-b)

      [(make-section target-a target-b)
       (vec (concat (sectionize mono-l) (sectionize mono-r)))])))

(defn -main [& args]
  (let [[target rocks] (reconstruct-surface (read-surface-points))] 

      ; S - поверхность
      ; G - начальное состояние игры для анализа направления
      ; отрезки: L - места для посадки
      ;          R - опасные места, рядом с которыми не летаем

    (dump "power-cloud:" power-cloud)
    (dump "angle-cloud:" (take 5 (drop 5 angle-cloud-table)))
    (comment (dump "surface:" S))
    (dump "landings:" target)
    (dump "rocks:" (count rocks) rocks)
      
    (loop [l (read-lander)
           P (lookup-path l target)]
      (let [prediction (first P)
            control (second P)
            trouble (cond (nil? control) "END OF PATH"
                          (not-aligned l prediction) "DIVERGENCE")]

        (dump-lander "lander:" l)
        (dump-lander "prediction:" prediction)
        
        (if trouble
          (let [new-path (lookup-path l target)
                new-control (second new-path)]
            (dump trouble)
            (println (:angle new-control) (:power new-control))
            (recur (read-lander) (rest new-path)))
              
          (do (dump "ON THE COURSE")
              (println (:angle control) (:power control))
              (recur (read-lander) (rest P))))))))

