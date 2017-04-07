(ns render (:require [quil.core :as q]
                     [lander :as l]
                     [geometry :as g]))

(set! *warn-on-reflection* true)

(def ^:private ^:const space-width 7000)
(def ^:private ^:const space-height 3000) 

(def ^:private ^:const display-width 1500)
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
               (* factor-k (:k s))
               (* factor-x (:mx s))))

(defn- invert-y [^double y] (- display-height y))

(defn- correct-y-section [s]
  (g/->Section (:ax s) (invert-y (:ay s))
               (:bx s) (invert-y (:by s))
               (:k s)
               (:mx s)))

(defn- correct-surface [sections]
  (map (comp correct-y-section scale-section) sections))

(defn- correct-lander [l]
  (l/->Lander (* factor-x (:x l))
              (invert-y (* factor-y (:y l)))
              (* factor-x (:vx l))
              (- (* factor-y (:vy l)))
              (:fuel l)
              (:control l)))

(defn- correct-trace [L]
  (let [l (last L)]
    {:trace (map correct-lander L)
     :mark  (str (apply format "%.3f|%.3f|%d|%d" ((juxt :vx :vy (comp :angle :control) (comp :power :control)) l)))}))

(defn update-scene [tag value]
  (swap! scene assoc tag (case tag
                           :surface (correct-surface value)
                           :shell (correct-surface value)
                           :landing-pad (correct-y-section (scale-section value))
                           :traces (map correct-trace value))
                     :redraw true)
  true)

(defn- draw-lander [l]
  (let [c  (:control l)
        x  (:x l)
        y  (:y l)
        vx (:vx l)
        vy (:vy l)
        ax (* (:power c) (Math/sin (Math/toRadians (+ 0 (:angle c)))))
        ay (* (:power c) (Math/cos (Math/toRadians (+ 0 (:angle c)))))]
    (q/no-stroke)
    (q/fill 0)
    (q/ellipse (:x l) (:y l) 4 4)
    (q/stroke 0 0 255)
    (q/line x y (+ x vx) (+ y vy))
    (q/stroke 255 0 0)
    (q/line x y (+ x (* 4 ax)) (+ y (* 4 ay))))) 

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
                  (q/line (:ax s) (:ay s) (:bx s) (:by s)))
                (if-let [landing (:landing-pad sc)]
                  (do (q/stroke 127)
                      (q/stroke-weight 1)
                      (doseq [s shell]
                        (let [x (if (<= (:ax s) (:ax landing)) (:bx s) (:ax s))]
                          (q/line x 0 x (- display-height 1))))))))

          (if-let [landing (:landing-pad sc)]
            (do (q/stroke 255 0 0)
                (q/stroke-weight 1)
                (let [delta (/ display-height 128)
                      mark-point (fn [x y] (q/line x (- y delta) x (+ y delta)))]
                  (mark-point (:ax landing) (:ay landing))
                  (mark-point (:bx landing) (:by landing)))))

          (if-let [traces (:traces sc)]
            (doseq [t traces]
              (doseq [l (:trace t)] (draw-lander l))
              (let [l (last (:trace t))
                    m (:mark t)]
                (q/text m (+ (:x l) 5) (- (:y l) 10))))))))
  
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
