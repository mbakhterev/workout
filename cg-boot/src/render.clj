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

(def ^:private ^:const display-width 1800)
(def ^:private ^:const display-height (long (* display-width (/ space-height space-width))))

(def ^:private ^:const factor-x (float (/ display-height space-height)))
(def ^:private ^:const factor-y (float (/ display-width space-width)))
(def ^:private ^:const factor-k (/ factor-y factor-x))

(def ^:private scene (atom {}))

(defn- scale-section [^records.Section s]
  (r/->Section (* factor-x (:ax s))
               (* factor-y (:ay s))
               (* factor-x (:bx s))
               (* factor-y (:by s))
               (* factor-k (:k s))
               (* factor-x (:mx s))))

(defn- correct-y-section [^records.Section s]
  (r/->Section (:ax s) (- (- display-height 1) (:ay s))
               (:bx s) (- (- display-height 1) (:by s))
               (:k s)
               (:mx s)))

(defn update-scene [tag value]
  (case tag
    :surface (swap! scene assoc :surface (map (comp correct-y-section scale-section) value))
    :landing-pad (swap! scene assoc :landing-pad (correct-y-section (scale-section value)))))

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
          )
      )
    )) 

(defn- setup []
;  (q/clear)
  (q/background 0)
  (q/frame-rate 1))

(defn start-sketch []
  (q/defsketch lander-debug
    :host "lander"
    :size [display-width display-height]
    :setup setup
    :draw draw))
