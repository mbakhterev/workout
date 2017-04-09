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

(defn- make-lander [L] (apply ->Lander (conj (vec (take 5 L))
                                             (apply ->Control (drop 5 L))
                                             0.0)))

(def ^:private ^:const test-id 0)
(def ^:private ^:const s-points (surface-points (:surface (test-data test-id))))
(def ^:private ^:const i-lander (make-lander (:lander (test-data test-id))))
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

(identity (map (juxt :stage :left?) stages))

(def ^:private ^:const control-cloud (for [a (range -90 91 15) p (range 0 5)] (->Control a p)))

(r/update-scene :traces (concat (map (fn [c] (take-while (partial alive? shell)
                                                         (iterate (partial move c 1.0) i-lander)))
                                     control-cloud)

                                (comment (model-control (search-path stages i-lander) stages i-lander))))

(take-while (partial alive? shell) (iterate (partial move (->Control 90 4) 1.0) i-lander))

(over-section? i-lander (:section (first stages)))

(align-control i-lander (first stages) (->Control 90 4))

(count control-cloud)

(def ^:private step-1 (get-landers false (reduce (partial integrate-hover (first stages) i-lander) {} control-cloud)))

(def ^:private step-2 (get-landers false (reduce (partial integrate-hover (second stages) (first step-1)) {} control-cloud)))

(def ^:private step-3 (get-landers false (reduce (partial integrate-hover (nth stages 3) (first step-2)) {} control-cloud)))

(identity step-1)



(def ^:private ^:const bad
  {:C (->Control 60 4)
   :S (nth stages 2) 
   :L #lander.Lander{:x 1500.0, :y 2514.4499999999994, :vx 100.0, :vy -37.10999999999999, :fuel 800, :control #lander.Control{:angle -15, :power 0}}})

(solve-hover (align-control (:L bad) (:S bad) (:C bad)) (:S bad))

(integrate-hover (:S bad) (:L bad) {} (:C bad))

(align-control (:L bad) (:S bad) (:C bad))

(solve-square-equation (* 0.5 -3.46) 91.68 -1511.53)

(let [r nil tl nil tr nil] (if-let [x (and r (if (<= 0.0 tl) tl (if (<= 0.0 tr) tr)))] x))
