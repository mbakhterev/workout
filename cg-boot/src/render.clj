(ns render (:require [quil.core :as q]))

(defn- draw []
  (q/stroke (random 255))
  (q/stroke-weight (random 20))
  (q/fill (random 255))

  (let [diam (q/random 100)
        x    (q/random (width))
        y    (q/random (height))]
    (q/ellipse x y diam diam)))

(defn- setup []
  (q/smooth)
  (q/background 255)
  (q/frame-rate 1))

(def ^:private ^:const display-width 1400)
(def ^:private ^:const display-height 600)
(def ^:private ^:const space-width 7000)
(def ^:private ^:const space-height 3000)

(q/defsketch lander-debug
  :host "lander"
  :size [display-width display-height]
  :setup setup
  :draw draw)
