(ns main (:gen-class)
         (:require [lander :refer :all]
                   [geometry :refer :all]
                   [render :as r]))

(set! *warn-on-reflection* true)

(defn- dump [& args] (binding [*out* *err*] (apply println args)))
(defn- read-surface [] (let [N (read)] (doall (repeatedly (* 2 N) read))))
(defn- read-lander [] (doall (repeatedly 7 read)))

(defn- make-guide [lander-data surface-data]
  (let [s-points (surface-points surface-data)
        i-lander (form-lander lander-data)
        l-pad (find-landing-pad s-points)
        surface (surface-sections s-points)
        [l-rock r-rock] (surface-shell s-points l-pad)
        stages (detect-stages i-lander l-rock l-pad r-rock)]
    (do (r/update-scene :surface surface)
        (r/update-scene :landing-pad l-pad)
        (r/update-scene :shell (vec (concat l-rock (list l-pad) r-rock))) 
        (r/update-scene :stages stages))
    (let [guide (model-control (search-guide stages i-lander) i-lander)]
      (do (r/update-scene :guide-traces guide))
      (vec (apply concat guide)))))

(defn- approximate-move [^lander.Control control trace]
  (let [l (move control 1.0 ^Lander (last trace))]
    (conj trace (assoc l :x (Math/round ^double (:x l))
                         :y (Math/round ^double (:y l))
                         :vx (Math/round ^double (:vx l))
                         :vy (Math/round ^double (:vy l))))))

; Тестовые данные
(def ^:private ^:const test-data [{:surface [0 1000 300 1500 350 1400 500 2000
                                             800 1800 1000 2500 1200 2100 1500 2400
                                             2000 1000 2200 500 2500 100 2900 800
                                             3000 500 3200 1000 3500 2000 3800 800
                                             4000 200 5000 200 5500 1500 6999 2800]
                                   :lander [500 2700 100 0 800 -90 0]}
                                  
                                  {:surface [0 1000 300 1500 350 1400 500 2100
                                             1500 2100 2000 200 2500 500 2900 300 3000
                                             200 3200 1000 3500 500 3800 800 4000 200
                                             4200 800 4800 600 5000 1200 5500 900
                                             6000 500 6500 300 6999 500]
                                   
                                   :lander [6500 2700 -50 0 1000 90 0]}])

(defn -main [& args]
  (comment (r/sketch-up))

  (let [; P (read-surface)
        ; L (read-lander)
        L (:lander (test-data 1))
        S (:surface (test-data 1))
        G (future (make-guide L S))]
    (dump "surface: " S)
    (dump "lander: " L)

    (let [[guide trace-04] (loop [t [(form-lander L)] g (deref G 128 nil)]
                             (if g
                               [g t]
                               (recur (approximate-move (->Control 0 4) t)
                                      (deref G 128 nil))))]
      (dump "guide-length:" (count guide))
      (dump "trace:" trace-04)
      (r/update-scene :lander-traces [trace-04])
      
      (comment (let [trace (loop [g guide
                         t [(last trace-04)]]
                    (if (empty? g)
                      t
                      (let [[l guide-item] (along-guide (last t) g)
                            guide-rest (rest (drop-while (fn [x] (not= guide-item x)) g))
                            l-next (approximate-move (:control l) t)]
                        (recur guide-rest l-next))))]
        (r/update-scene :lander-traces [trace-04 trace])))

      (let [trace (loop [t [(last trace-04)]]
                    (let [control (along-guide (last t) guide)]
                      (if (nil? control)
                        t
                        (recur (approximate-move control t)))))]
        (r/update-scene :lander-traces [trace-04 trace]))
      
      (comment (along-guide (last trace-04) guide)))))  

; eval

(-main)

(comment (r/sketch-up)
         (def ^:private ^:const bad-cases
           [{:C (->Control 60 4)
             :S (nth stages 2) 
             :L #lander.Lander{:x 1500.0, :y 2514.4499999999994, :vx 100.0, :vy -37.10999999999999, :fuel 800,
                               :control #lander.Control{:angle -15, :power 0}}}

            {:C (->Control 20 4)
             :S (nth stages 2)
             :L #lander.Lander{:x 1500.3393543299987, :y 2529.3060059296076, :vx 99.64872884586892, :vy -27.17071128706871, :fuel 790,
                               :control #lander.Control{:angle 5, :power 4}}}])

         (def ^:private ^:const bad (nth bad-cases 1)) 

         (map :stage stages)
         (map count guide-controls)
         (map :stage stages)
         (last guide-controls)
         (time (search-guide stages i-lander))
         (identity stages)
         (identity i-lander)

         (solve-descend-one (:lander (first (last guide-controls))) (last stages))

         (map (fn [s] [(:x (first s)) (:x (second s))]) (partition 2 1 s-points))
         (map (fn [[a b]] (println "a:" a "b:" b) (make-section a b)) (partition 2 1 s-points))
         (map :x-goal (hover-stages i-lander l-pad l-shell r-shell (list)))
         (identity stages)
         (map (juxt :stage :x-goal) stages))
