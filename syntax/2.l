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

Или

(defn- ^Lander move (^Lander l ^long angle ^double power)
  (let (x    (:x l)
        y    (:y l)
        dx   (:dx l)
        dy   (:dy l)
        fuel (:fuel l)
        ddx  (* power (x-power angle))
        ddy  (* power (y-power angle)))
    (->Lander (+ x dx (* 0.5 ddx)) (+ y dy (* 0.5 ddy)) (+ dx ddx) (+ dy ddy)
              (- fuel power) angle power)))


Разница, конечно есть в look-and-feel. Но она субкритична. Ещё примеры:

(defn- ^double x-power [^long phi] (nth cos-table (+ phi 90)))
(defn- ^double y-power [^long phi] (nth sin-table (+ phi 90)))

(defn- ^double fitness [^Lander l ^Section target ^double energy] 0.0)

Или

(defn- ^double x-power (^long phi) (nth cos-table (+ phi 90)))
(defn- ^double y-power (^long phi) (nth sin-table (+ phi 90)))

(defn- ^double fitness (^Lander l ^Section target ^double energy) 0.0)
