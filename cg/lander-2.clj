(ns Player (:gen-class))

(defn- height [surface x y]
  (let [[xa xb ya yb] (for [i [first second] j [first second]] (comp i j))
        cut           (first (filter (fn [p] (<= (xa p) x (xb p))) surface))
        [x1 y1 x2 y2] ((juxt xa ya xb yb) cut)
        k             (float (/ (- y2 y1) (- x2 x1)))
        yp            (+ y1 (* k (- x x1)))]
    (- y yp)))

(def ^:const M 3.711)

(defn- integrate [[h v] p]
  (let [a (- M p)] [(+ h v (* 0.5 a a)) (+ v a)]))

(defn- solve-sq [a b c]
  (let [D (- (* b b) (* 4.0 a c))]
    (if (< D 0)
      nil
      (let [sqrt-D (Math/sqrt D) q (* 2 a)]
        (mapv (fn [op] (/ (op (- b) sqrt-D) q)) [+ -])))))

(defn- thrust [H V P]
  (let [[h v] (reduce integrate [H (- V)] (range P 4))
        a (- M 4)
        [tp tm] (solve-sq (* 0.5 a) v (- h))]
    (binding [*out* *err*]
      (println "HVP:" [H V P] "hv:" [h v] "times:" [tp tm]))
    (if (and tp (< 37 (+ v (* a tp)))) 4 0)))


(comment (defn -main [& args]
  (let [surface (partition 2 1 (partition 2 (repeatedly (* 2 (read)) read)))]
    (binding [*out* *err*] (println "Surface:" surface))
    (loop [hold 0]
      (let [[X Y dx dy fuel rotate power :as game] (vec (repeatedly 7 read))
            h (height surface X Y)]
        (binding [*out* *err*] (println "height:" h "thurst:" power)) 
        (println (str 0 \space (thrust h dy power))) 
      (recur (inc hold)))))))

(defrecord Point [^double x ^double y])
(defrecord Section [^Point a ^Point b])

(defrecord Lander [^double x ^double y ^double dx ^double dy
                   ^double fuel ^double angle ^double power])

(comment (defn- read-surface []
  (->> (apply list (repeatedly (* 2 (read)) read))
       (partition 2)
       (map (fn [p] (apply assoc {} (interleave (list :x :y) (floats p)))))
       (partition 2 1)
       (map (fn [c] (apply assoc {} (interleave (list :a :b) c)))))))

(defn- read-surface []
  (->> (apply list (repeatedly (* 2 (read)) read))
       (partition 2)
       (map (fn [p] (apply ->Point p)))
       (partition 2 1)
       (map (fn [c] (apply ->Section c)))))

(defn- read-game []
  (apply assoc {} (interleave (list :x :y :dx :dy :fuel :angle :power)
                              (repeatedly 7 read))))

(defn- read-lander [] (apply ->Lander (repeatedly 7 read)))

(defn- dump [& args] (binding [*out* *err*] (apply println args)))

(defn- find-landing [S]
  (let [LR (group-by (fn [c] (< -0.01 (- (-> c :a :y) (-> c :b :y)) 0.01)) S)]
    [(LR true) (LR false)]))

; Движение модуля l при управлении (vec angle power). Сохраняем новое положение
; модуля и то управление, которое привело его в это положение. Положение - это
; вектор в фазовом пространстве (vec x y dx dy fuel). Нужно быстро считать,
; поэтому juxt не используем.

; ddx и ddy, потому что угол отсчитывается от оси (+ PI/2)

(defn- move [^Lander l ^double angle ^double power]
  (let [x    (:x l)
        y    (:y l)
        dx   (:dx l)
        dy   (:dy l)
        fuel (:fuel l)
        phi  (Math/toRadians angle)
        ddx  (* power (Math/sin (- phi)))
        ddy  (- (* power (Math/cos phi)) M)]
    (->Lander (+ x dx (* 0.5 ddx)) (+ y dy (* 0.5 ddy)) (+ dx ddx) (+ dy ddy)
              (- fuel power) angle power)))

(defn -main [& args]
  (let [S (read-surface)      
        G (read-lander)         
        [L R] (find-landing S)] 

      ; S - поверхность
      ; G - начальное состояние игры для анализа направления
      ; отрезки: L - места для посадки
      ;          R - опасные места, рядом с которыми не летаем


    (dump "surface:" S)
    (dump "game:" G)
    (dump "landings:" L)
    (dump "rocks:" R)
    
    (loop [game G] 
      ; (dump game)
      (println 81 4)

      (let [next-game (read-lander)
            model     (move game (:angle next-game) (:power next-game))]
        (dump "model:" (map (comp (partial format "%.02f") double second) model))
        (dump "ngame:" (map (comp (partial format "%.02f") double second) next-game))

        (recur next-game)))))
