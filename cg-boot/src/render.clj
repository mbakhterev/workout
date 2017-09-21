(ns render (:require [quil.core :as q]
                     [lander :as l]
                     [geometry :as g]))

(set! *warn-on-reflection* true)

(def ^:private ^:const space-width 7000)
(def ^:private ^:const space-height 3000) 

(def ^:private ^:const display-width (- 1600 32))
(def ^:private ^:const display-height (long (* display-width (/ space-height space-width))))

(def ^:private ^:const factor-x (float (/ display-height space-height)))
(def ^:private ^:const factor-y (float (/ display-width space-width)))
(def ^:private ^:const factor-k (/ factor-y factor-x))

(def ^:private scene (atom {}))

(defn- scale-section [s]
  (g/->Section (* factor-x (:ax s))
               (* factor-y (:ay s))
               (* factor-x (:bx s))
               (* factor-y (:by s))
               (* factor-x (:nx s))
               (* factor-y (:ny s))))

(defn- invert-y [^double y] (- display-height y))

(defn- invert-y-section [s]
  (g/->Section (:ax s) (invert-y (:ay s))
               (:bx s) (invert-y (:by s))
               (:nx s)
               (:ny s)))

(defn- reshape-surface [sections]
  (map (comp invert-y-section scale-section) sections))

(defn- reshape-lander [l]
  (l/->Lander (* factor-x (:x l))
              (invert-y (* factor-y (:y l)))
              (* factor-x (:vx l))
              (- (* factor-y (:vy l)))
              (:fuel l)
              (:control l)))

(defn- reshape-trace-chunk [full-mark? L]
  (assert (not (empty? L)))
  (let [l (last L)]
    {:trace (map reshape-lander L)
     :mark  (let [m (format "%d|%d" (:angle (:control l)) (:power (:control l)))]
              (str (if full-mark? (format "%d|%.3f|%.3f|" (:fuel l) (:vx l) (:vy l))) m))}))

(defn- reshape-trace [trace]
  (if (not (empty? trace))
    (concat (map (partial reshape-trace-chunk false) (drop-last trace))
            (list (reshape-trace-chunk true (last trace))))))

(defn- reshape-stage [^geometry.Stage s]
  (if (#{:hover :brake :reverse} (:stage s))
    {:target (* factor-x (:x-target s))
     :mark (str (:stage s) (if (pos? (:direction s)) " left" " right"))
     :left? (pos? (:direction s))}))

(defn update-scene [tag value]
  (swap! scene assoc tag (case tag
                           :surface (reshape-surface value)
                           :shell (reshape-surface value)
                           :landing-pad (invert-y-section (scale-section value))
                           :stages (if value (keep reshape-stage value))  
                           :guides (keep reshape-trace value)
                           :traces (if (not (empty? value))
                                     (reshape-trace (filter (comp not empty?) (conj (map next (next value)) (first value))))))
                     :redraw true)
  true)

(defn- draw-lander [^lander.Lander {x :x y :y vx :vx vy :vy c :control} color ^long r]
  (let [ax (* (:power c) (Math/sin (Math/toRadians (+ 0 (:angle c)))))
        ay (* (:power c) (Math/cos (Math/toRadians (+ 0 (:angle c)))))]
    (apply q/stroke color)
    (q/line x y (+ x (* 4 ax)) (+ y (* 4 ay)))
    (apply q/stroke color)
    (q/line x y (+ x vx) (+ y vy))     
    (if (pos? r)
      (do (q/no-stroke)
          (q/fill 0)
          (q/ellipse x y r r))
      (do (q/stroke 0)
          (q/fill 255)
          (q/ellipse x y r r)))))

(defn- draw-trace [color trace]
  (doseq [t trace]
    (assert (not (empty? t)))
    (doseq [l (drop-last (:trace t))] (draw-lander l color 4))
    (let [l (last (:trace t))
          m (:mark t)
          text-width (q/text-width m)]
      (draw-lander l color -4)
      (q/fill 0)
      (q/text m (- (:x l) 5 text-width) (+ (:y l) 10)))))

(def ^:private ^:const rG 10.0)

(def ^:private ^:const x-cell-rG (* (- rG 3) factor-x))
(def ^:private ^:const y-cell-rG (* (- rG 3) factor-y))

(defn- draw-cell [c]
  (let [mx (:x c)
        my (:y c)]
    (q/stroke (- 256 128))
    (q/no-fill)
    (q/rect (- mx x-cell-rG) (- my y-cell-rG) (* 2 x-cell-rG) (* 2 y-cell-rG)))) 

(defn- draw-grid [G]
  (q/stroke 128)
  (q/stroke-weight 2)
  (let [dx (* factor-x (:dG G))
        dy (- (* factor-y (:dG G)))
        rx (* dx 0.5)
        ry (* dy 0.5)
        r  (first (:rows G))]
    (if r (loop [x (+ (:left r) rx)
                 n (count (:cells r))
                 y (+ (:baseline G) ry)
                 R (rest (:rows G))]
            (cond (> n 0) (do (q/point x y)
                              (recur (+ x dx) (- n 1) y R))

                  (not (empty? R)) (let [nr (first R)]
                                     (recur (+ (:left nr) rx) (count (:cells nr)) (+ y dy) (rest R))))))))

(defn- draw []
  (let [sc (deref scene)]
    (if (:redraw sc)
      (println "redrawing")
      (do (q/background 255)
          (if-let [surface (:surface sc)]
            (do (q/stroke 127)
                (q/stroke-weight 1)
                (doseq [s surface]
                  (q/line (:ax s) (:ay s)
                          (:bx s) (:by s)))))

          (if-let [landing (:landing-pad sc)]
            (do (q/stroke 255 0 0)
                (q/stroke-weight 1)
                (let [delta (/ display-height 128)
                      mark-point (fn [x y] (q/line x (- y delta) x (+ y delta)))]
                  (mark-point (:ax landing) (:ay landing))
                  (mark-point (:bx landing) (:by landing)))))

          (if-let [shell (:shell sc)]
            (do (q/stroke 0)
                (q/stroke-weight 1)
                (doseq [s shell]
                  (q/line (:ax s) (:ay s) (:bx s) (:by s)))))

          (if-let [stages (:stages sc)]
            (do (q/stroke 127)
                (q/stroke-weight 1)
                (q/fill 0)
                (doseq [{x :target t :mark l :left?} stages]
                  (q/line x 0 x (- display-height 1))
                  (if l
                    (q/text t (- x 7.5 (q/text-width t)) 10)
                    (q/text t (+ x 7.5) 10)))))

          (if-let [landing (:landing-pad sc)]
            (do (q/stroke 255 0 0)
                (q/stroke-weight 1)
                (let [delta (/ display-height 128)
                      mark-point (fn [x y] (q/line x (- y delta) x (+ y delta)))]
                  (mark-point (:ax landing) (:ay landing))
                  (mark-point (:bx landing) (:by landing)))))

          (if-let [traces (:guides sc)]
            (doall (map (partial draw-trace [0 0 255]) traces)))

          (if-let [trace (:traces sc)]
            (draw-trace [255 0 0] trace)))))

  (swap! scene assoc :redraw false)) 

(defn- setup []
  (q/smooth)
  (q/text-font (q/create-font "DejaVu Sans Mono" 8 true))
  (q/background 255)
  (q/frame-rate 1))

(defn sketch-up []
  (q/defsketch lander-debug
    :host "lander"
    :size [display-width display-height]
    :setup setup
    :draw draw))
