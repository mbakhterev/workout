(comment [lander :as l])

(ns render (:require [quil.core :as q]))

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
;  (q/background 255)
  (q/frame-rate 1))

(def ^:private ^:const display-width 700)
(def ^:private ^:const display-height 300)
(def ^:private ^:const space-width 7000)
(def ^:private ^:const space-height 3000)

(def ^:private ^:const factor-x (float (/ display-height space-height)))
(def ^:private ^:const factor-y (float (/ display-width space-width)))
(def ^:private ^:const factor-k (/ factor-y factor-x))

(def ^:private scene (atom {}))

(defn- scale-section [^lander.Section s]
  (l/->Section (* factor-x (:ax s))
               (* factor-y (:ay s))
               (* factor-x (:bx s))
               (* factor-y (:by s))
               (* factor-k (:k s))
               (* factor-x (:mx s))))

(defn update-scene [tag value]
  (case tag
    :surface (swap! scene assoc :surface (map scale-section value))))

(defn- draw []
  (let [S (deref scene)]
    (if-let [surface (:surface S)]
      (do (q/stroke 0)
          (q/stroke-weight 4)
          (doseq [s surface]
            (q/line (:ax s) (- display-height (:ay s))
                    (:bx s) (- display-height (:by s))))))))

(defn- setup [] (q/frame-rate 1))

(q/defsketch lander-debug
  :host "lander"
  :size [display-width display-height]
  :setup setup
  :draw draw)
