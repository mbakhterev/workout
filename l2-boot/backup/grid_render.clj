(ns grid-render (:require [grid :refer :all]
                          [geometry :refer :all]))

(defn- correct-cell [c factor-x invert-y factor-y]
  (->Point (* factor-x (:x c)) (invert-y (* factor-y (:y c)))))

(defn- correct-row [R factor-x] (->Row (* factor-x (:left R)) (:cells R)))

(defn- correct-grid [G invert-y factor-y]
  (->Grid (:dG G)
          (:dV G)
          (:nV G)
          (invert-y (* factor-y (:baseline G)))
          (mapv correct-row (:rows G)))) 
