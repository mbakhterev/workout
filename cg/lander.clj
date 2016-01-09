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

(defn- thrust [h dh p]
  (let [)

(defn -main [& args]
  (let [surface (partition 2 1 (partition 2 (repeatedly (* 2 (read)) read)))]
    (binding [*out* *err*] (println "Surface:" surface))
    (while true
      (let [[X Y x-speed y-speed fuel rotate power :as game] (vec (repeatedly 7 read))
            h (height surface X Y)]
        (binding [*out* *err*] (println "height:" h))
        (println "0 3")))))

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
