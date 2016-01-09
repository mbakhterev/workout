(ns Player (:gen-class))

(defn- height [surface x y]
  (let [xa (comp first first)
        xb (comp first second)
        ya (comp second first)
        yb (comp second second)
        cut (first (filter (fn [p] (<= (xa p) x (xb p))) surface))
        x1 (xa cut)
        y1 (ya cut)
        x2 (xb cut)
        y2 (yb cut)
        k (float (/ (- y2 y1) (- x2 x1)))
        yp (+ y1 (* k (- x x1)))]
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
;        h (- H pre-h)
        a (- M 4)
        [tp tm] (solve-sq (* 0.5 a) v (- h))]
    (binding [*out* *err*]
      (println "HVP:" [H V P] "hv:" [h v] "times:" [tp tm]))
    (if (and tp (< 37 (+ v (* a tp)))) 4 0)))

(defn -main [& args]
  (let [surface (partition 2 1 (partition 2 (repeatedly (* 2 (read)) read)))]
    (binding [*out* *err*] (println "Surface:" surface))
    (loop [hold 0]
      (let [[X Y dx dy fuel rotate power :as game] (vec (repeatedly 7 read))
            h (height surface X Y)]
        (binding [*out* *err*] (println "height:" h "thurst:" power))
        (println (str 0 \space (thrust h dy power)))
      (recur (inc hold))))))

; Auto-generated code below aims at helping you parse
; the standard input according to the problem statement.

(defn -main-prime [& args]
  (let [surfaceN (read)]
    ; surfaceN: the number of points used to draw the surface of Mars.
    (loop [i surfaceN]
      (when (> i 0)
        (let [landX (read) landY (read)]
          ; landX: X coordinate of a surface point. (0 to 6999)
          ; landY: Y coordinate of a surface point. By linking all the points together in a sequential fashion, you form the surface of Mars.
        (recur (dec i)))))
    (while true
      (let [X (read) Y (read) hSpeed (read) vSpeed (read) fuel (read) rotate (read) power (read)]
        ; hSpeed: the horizontal speed (in m/s), can be negative.
        ; vSpeed: the vertical speed (in m/s), can be negative.
        ; fuel: the quantity of remaining fuel in liters.
        ; rotate: the rotation angle in degrees (-90 to 90).
        ; power: the thrust power (0 to 4).
        
        ; (binding [*out* *err*]
        ;   (println "Debug messages..."))
        
        ; 2 integers: rotate power. rotate is the desired rotation angle (should be 0 for level 1), power is the desired thrust power (0 to 4).
        (println "0 4")))))
