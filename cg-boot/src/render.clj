(ns render (:require [quil.core :as q]
                     [records :as r]))

(defn- test-draw []
  (q/stroke (q/random 255))
  (q/stroke-weight (q/random 20))
  (q/fill (q/random 255))

  (let [diam (q/random 100)
        x    (q/random (q/width))
        y    (q/random (q/height))]
    (q/ellipse x y diam (* 2 diam))))

(defn- test-setup []
  (q/smooth)
  (q/frame-rate 1))

(def ^:private ^:const space-width 7000)
(def ^:private ^:const space-height 3000) 

(def ^:private ^:const display-width 1500)
(def ^:private ^:const display-height (long (* display-width (/ space-height space-width))))

(def ^:private ^:const factor-x (float (/ display-height space-height)))
(def ^:private ^:const factor-y (float (/ display-width space-width)))
(def ^:private ^:const factor-k (/ factor-y factor-x))

(def ^:private scene (atom {}))

(defn- ^records.Lander scale-section [^records.Section s]
  (r/->Section (* factor-x (:ax s))
               (* factor-y (:ay s))
               (* factor-x (:bx s))
               (* factor-y (:by s))
               (* factor-k (:k s))
               (* factor-x (:mx s))))

(defn- invert-y [^double y] (- display-height y))

(defn- ^records.Lander correct-y-section [^records.Section s]
  (r/->Section (:ax s) (invert-y (:ay s))
               (:bx s) (invert-y (:by s))
               (:k s)
               (:mx s)))

(defn- correct-surface [sections]
  (map (comp correct-y-section scale-section) sections))

(defn- ^records.Lander correct-lander [^records.Lander l]
  (r/->Lander (* factor-x (:x l))
              (invert-y (* factor-y (:y l)))
              (* factor-x (:vx l))
              (- (* factor-y (:vy l)))
              (:fuel l)
              (:angle l)
              (:power l)))

(defn update-scene [tag value]
  (swap! scene assoc tag (case tag
                           :surface (correct-surface value)
                           :shell (correct-surface value)
                           :landing-pad (correct-y-section (scale-section value))
                           :lander (map correct-lander value))))

(defn- draw []
  (q/clear)
  (let [sc (deref scene)]
    (if-let [surface (:surface sc)]
      (do (q/stroke 127)
          (q/stroke-weight 1)
          (doseq [s surface]
            (q/line (:ax s) (:ay s)
                    (:bx s) (:by s)))))

    (if-let [landing (:landing-pad sc)]
      (do (q/stroke 255)
          (q/stroke-weight 1)
          (let [delta (/ display-height 128)
                mark-point (fn [x y] (q/line x (- y delta) x (+ y delta)))]
            (mark-point (:ax landing) (:ay landing))
            (mark-point (:bx landing) (:by landing)))))
    
    (if-let [shell (:shell sc)]
      (do (q/stroke 255)
          (q/stroke-weight 1)
          (doseq [s shell]
            (q/line (:ax s) (:ay s)
                    (:bx s) (:by s)))))
    
    (if-let [trace (:lander sc)]
      (doseq [lander trace]
        (let [x (:x lander)
              y (:y lander)
              ax (* (:power lander) (Math/sin (:angle lander)))
              ay (* (:power lander) (Math/cos (:angle lander)))]
          (q/no-stroke)
          (q/fill 0 255 0)
          (q/ellipse (:x lander) (:y lander) 8 8)
          (q/stroke 255 0 0)
          (q/stroke-cap :project)
          (q/line x y (+ x ax) (+ y ay))
          (q/stroke 0 0 255)
          (q/line x y (+ x (:vx lander)) (+ y (:vy lander)))))))) 

(defn- setup []
  (q/background 0)
  (q/frame-rate 1))

(defn start-sketch []
  (q/defsketch lander-debug
    :host "lander"
    :size [display-width display-height]
    :setup setup
    :draw draw))
