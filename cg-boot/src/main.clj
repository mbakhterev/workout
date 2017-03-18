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
  (def ^:private ^:const shell (vec (concat l r)))
  (def ^:private ^:const l-shell l)
  (def ^:private ^:const r-shell r))
 
(r/update-scene :surface surface)
(r/update-scene :landing-pad l-pad)
(r/update-scene :shell shell) 

(r/update-scene :traces (list {:trace (take-while (partial alive? shell)
                                                  (reductions (wrap 1.0 move)
                                                              i-lander (repeat [90 4])))
                               :mark \D}

                              {:trace (integrate-hover (first (detect-stages i-lander
                                                                             l-shell l-pad r-shell))
                                                       i-lander 0 4)}))

(count (detect-stages i-lander l-shell l-pad r-shell))

(integrate-hover (first (detect-stages i-lander l-shell l-pad r-shell)) i-lander 90 0)
(take-while (partial alive? shell) (reductions (wrap 0.5 move) i-lander (repeat [90 4])))

(move -90 0 0.4 i-lander)
