(ns main (:gen-class)
         (:require [lander :refer :all]
                   [geometry :refer :all]
                   [render :as r]))

(defn- dump [& args] (binding [*out* *err*] (apply println args)))
(defn- read-surface [] (let [N (read)] (doall (repeatedly (* 2 N) read))))
(defn- read-lander [] (doall (repeatedly 7 read)))

(defn -main [& args]
  (let [P (read-surface)
        L (read-lander)]
    (dump "surface: " P)
    (dump "lander: " L)))

; Тестовые данные
(def ^:private ^:const test-data [{:surface [0 1000 300 1500 350 1400 500 2000
                                             800 1800 1000 2500 1200 2100 1500 2400
                                             2000 1000 2200 500 2500 100 2900 800
                                             3000 500 3200 1000 3500 2000 3800 800
                                             4000 200 5000 200 5500 1500 6999 2800]
                                   :lander [500 2700 100 0 800 -90 0]}])

(def ^:private ^:const test-id 0)
(def ^:private ^:const s-points (surface-points (:surface (test-data test-id))))
(def ^:private ^:const i-lander (apply ->Lander (conj (:lander (test-data test-id))))) 
(def ^:private ^:const l-pad (find-landing-pad s-points))
(def ^:private ^:const surface (surface-sections s-points))
(let [[l r] (surface-shell s-points l-pad)]
  (def ^:private ^:const shell (vec (concat l (list l-pad) r)))
  (def ^:private ^:const l-shell l)
  (def ^:private ^:const r-shell r))
 
(r/update-scene :surface surface)
(r/update-scene :landing-pad l-pad)
(r/update-scene :shell shell) 

(def ^:private ^:const stages (detect-stages i-lander l-shell l-pad r-shell))

(defn- trace-control [l angle power]
  (take-while (partial alive? shell) (reductions (wrap 1.0 move) l (repeat [angle power]))))

(r/update-scene
  :traces (concat (comment (list {:trace (trace-control i-lander 90 4) :mark \D}
                                 {:trace (list (nth (solve-hover i-lander ((comp :bx :section) (first stages))) 1))}
                                 {:trace (trace-control (assoc i-lander :vx 0 :angle 90 :power 4) 90 4)}))
                  (for [p (range 4) a (range -90 91 5)] {:trace (trace-control i-lander a p)})))

(constraint (move 15 0 1.0 i-lander) l-pad)

(time (count (for [p (range 4) a (range -90 91 5)] {:trace (trace-control i-lander a p)})))

(integrate-hover l-pad (first stages) i-lander {} 0 4)

(identity i-lander)
