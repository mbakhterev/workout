(ns lander (:require [render :as r]
                     [records :refer :all]))

; Вспомогательные функции
(defn- dump [& args] (binding [*out* *err*] (apply println args)))
(defn- read-surface [] (let [N (read)] (doall (repeatedly (* 2 N) read))))
(defn- read-lander [] (doall (repeatedly 7 read)))

; Тестовые данные
(def ^:private ^:const test-data [{:surface [0 1000 300 1500 350 1400 500 2000
                                             800 1800 1000 2500 1200 2100 1500 2400
                                             2000 1000 2200 500 2500 100 2900 800
                                             3000 500 3200 1000 3500 2000 3800 800
                                             4000 200 5000 200 5500 1500 6999 2800]
                                   :lander [500 2700 100 0 800 -90 0]}])

(defn- surface-points [raw-numbers]
  (map (fn [p] (apply ->Point p)) (partition 2 raw-numbers)))

(defn- surface-sections [points]
  (map (fn [s] (apply make-section s)) (partition 2 1 points)))

(defn- find-landing-pad [points]
  (letfn [(^boolean is-pad ([[^Point a ^Point b]] (< -0.01 (- (:y a) (:y b)) 0.01)))]
    (first (filter is-pad (partition 2 1 points)))))

(find-landing-pad (surface-points (:surface (test-data 0))))

(r/update-scene :surface (surface-sections (surface-points (:surface (test-data 0))))) 

(r/update-scene :landing-pad (apply make-section (-> (:surface (test-data 0))
                                                     (surface-points)
                                                     (find-landing-pad))))

(r/update-scene :shell (surface-sections (surface-points (:surface (test-data 0)))))
