(ns render (:require '[quil.core]))

(defn- draw []
  (stroke (random 255))
  (stroke-weight (random 10))
  (fill (random 255))

  (let [diam (random 100)
        x    (random (width))
        y    (random (height))]
    (ellipse x y diam diam)))

(defn- setup []
  (smooth)
  (background 255)
  (frame-rate 1))

(defsketch lander-debug
  :host "lander"
  :size [1400 600]
  :setup setup
  :draw draw)
