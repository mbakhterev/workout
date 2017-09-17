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

(defn- correct-y-section [s]
  (g/->Section (:ax s) (invert-y (:ay s))
               (:bx s) (invert-y (:by s))
               (:nx s)
               (:ny s)))

(defn- correct-surface [sections]
  (map (comp correct-y-section scale-section) sections))

(defn- correct-lander [l]
  (l/->Lander (* factor-x (:x l))
              (invert-y (* factor-y (:y l)))
              (* factor-x (:vx l))
              (- (* factor-y (:vy l)))
              (:fuel l)
              (:control l)))

(defn- correct-trace [full-mark? L]
  (let [l (last L)]
    {:trace (map correct-lander L)
     :mark  (let [m (format "%d|%d" (:angle (:control l)) (:power (:control l)))]
              (str (if full-mark? (format "%d|%.3f|%.3f|" (:fuel l) (:vx l) (:vy l))) m))}))

(defn- correct-stage [^geometry.Stage s]
  (if (#{:hover :brake :reverse} (:stage s))
    {:target (* factor-x (:x-target s))
     :mark (str (:stage s) (if (pos? (:direction s)) " left" " right"))
     :left? (pos? (:direction s))}))

(defn- update-traces [traces]
  (concat (map (partial correct-trace false) (drop-last traces))
          (list (correct-trace true (last traces)))))

(defn update-scene [tag value]
  (swap! scene assoc tag (case tag
                           :surface (correct-surface value)
                           :shell (correct-surface value)
                           :landing-pad (correct-y-section (scale-section value))
                           :guide-traces (if (not (empty? value)) (update-traces value))
                           :lander-traces (if (not (empty? value)) (update-traces value))
                           :stages (if value (keep correct-stage value)))
                     :redraw true)
  true)

(defn- draw-lander [^lander.Lander l color ^long r]
  (let [c  (:control l)
        x  (:x l)
        y  (:y l)
        vx (:vx l)
        vy (:vy l)
        ax (* (:power c) (Math/sin (Math/toRadians (+ 0 (:angle c)))))
        ay (* (:power c) (Math/cos (Math/toRadians (+ 0 (:angle c)))))]
    (q/no-stroke)
    (q/fill 0)
    (q/ellipse (:x l) (:y l) r r)
    (apply q/stroke color)
    (q/line x y (+ x (* 4 ax)) (+ y (* 4 ay)))
    (apply q/stroke color)
    (q/line x y (+ x vx) (+ y vy)))) 

(defn- draw-traces [traces color]
  (doseq [t traces]
    (when (not (empty? t))
      (doseq [l (drop-last (:trace t))] (draw-lander l color 4))
      (let [l (last (:trace t))
            m (:mark t)
            text-width (q/text-width m)]
        (draw-lander l color 6)
        (q/text m (- (:x l) 5 text-width) (+ (:y l) 10))))))

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
  (q/stroke 255)
  (q/stroke-weight 3)
  (q/text "hello world" 500 500)

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
                  (comment (println "stage mark:" t))
                  (q/line x 0 x (- display-height 1))
                  (if l
                    (q/text t (- x 7.5 (q/text-width t)) 10)
                    (q/text t (+ x 7.5) 10))
                  (let [fy (- 10 (/ (+ (q/text-ascent) (* 0.0 (q/text-descent))) 2.0))]
                    (if l
                      (q/line (- x 5) fy x fy)
                      (q/line (+ x 5) fy x fy))))))

          (if-let [landing (:landing-pad sc)]
            (do (q/stroke 255 0 0)
                (q/stroke-weight 1)
                (let [delta (/ display-height 128)
                      mark-point (fn [x y] (q/line x (- y delta) x (+ y delta)))]
                  (mark-point (:ax landing) (:ay landing))
                  (mark-point (:bx landing) (:by landing)))))

          (if-let [traces (:guide-traces sc)]
            (draw-traces traces [0 0 255])
            (comment (doseq [t traces]
                       (doseq [l (:trace t)] (draw-lander l [0 0 255] [0 0 255]))
                       (let [l (last (:trace t))
                             m (:mark t)
                             text-width (q/text-width m)]
                         (q/text m (- (:x l) 5 text-width) (+ (:y l) 10))
                         (q/ellipse (:x l) (:y l) 6 6)))))

          (if-let [traces (:lander-traces sc)]
            (draw-traces traces [255 0 0])
            (comment (doseq [t traces]
                       (doseq [l (:trace t)] (draw-lander l [255 0 0] [255 0 0]))
                       (let [l (last (:trace t))
                             m (:mark t)
                             text-width (q/text-width m)]
                         (q/text m (- (:x l) 5 text-width) (+ (:y l) 10)))))))))

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
