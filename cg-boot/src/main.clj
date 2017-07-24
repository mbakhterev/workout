(ns main (:gen-class)
         (:require [lander :refer :all]
                   [geometry :refer :all]
                   [render :as r]))

(set! *warn-on-reflection* true)

(defn- dump [& args] (binding [*out* *err*] (apply println args)))
(defn- read-surface [] (let [N (read)] (doall (repeatedly (* 2 N) read))))
(defn- read-lander [] (doall (repeatedly 7 read)))

(defn- refine-guide [guide] (vec (apply concat (first guide) (map rest (rest guide)))))

(defn- sketch-landscape [^geometry.Landscape scape]
  (r/update-scene :surface (:raw-surface scape))
  (r/update-scene :landing-pad (:landing-pad scape)) 
  (r/update-scene :shell (apply concat ((juxt :left-rock :landing-pad :right-rock) ) l-rock (list l-pad) r-rock)))

(defn- make-guide [lander-data ^geometry.Landscape scape]
  (let [i-lander (form-lander lander-data)
        [l-rock l-pad r-rock] ((juxt :left-rock :landing-pad :right-rock) scape)
        stages (detect-stages i-lander scape)]
    (do (sketch-landscape scape)
        (r/update-scene :surface (:raw-surface scape))
        (r/update-scene :landing-pad (:landing-pad scape))
        (r/update-scene :shell (concat l-rock (list l-pad) r-rock)) 
        (r/update-scene :stages stages))
    (let [guide (model-control (search-guide stages i-lander) i-lander)]
      (do (r/update-scene :guide-traces guide))
      (refine-guide guide)
      (vec (apply concat (first guide) (map rest (rest guide)))))))

(let [traces (atom {:guide [] :lander []})]
  (defn- reset-traces [] (reset! traces {:guide [] :lander []})) 

  (defn- next-guide [g] (swap! traces (fn [t] (assoc t :guide (concat (:guide t) g))))) 
  (defn- next-trace [^lander.Lander l]
    (swap! traces (fn [t] (assoc t :lander (conj (:lander t) [l])))))

  (defn- unpack-traces [t] (and (not (empty? t))
                                (not (empty? (last t)))
                                (= lander.Lander (type (last (last t))))
                                [(drop-last t) (last t)]))

  (defn- sketch-traces []
    (let [t (deref traces)]
      (if (unpack-traces (:guide t)) (r/update-scene :guide-traces (:guide t)))
      (if (unpack-traces (:lander t)) (r/update-scene :lander-traces (:lander t)))))

  (defn- trace-move [^lander.Control control]
    (swap! traces (fn [T] (let [[previous current :as t] (unpack-traces (:lander T))]
                            (assert t)
                            (conj previous
                                  (conj current (move control 1.0 ^Lander (last current))))))))
  
  (defn- approximate-last []
    (let [[previous current :as t] (unpack-traces (:lander (deref traces)))]
      (assert t)
      (let [l ^lander.Lander (last current)]
        (assoc l
               :x (Math/round ^double (:x l))
               :y (Math/round ^double (:y l))
               :vx (Math/round ^double (:vx l))
               :vy (Math/round ^double (:vy l))))))) 

(defn- wait-loop [^geometry.Landscape scape
                  ^lander.Control control
                  ^lander.Lander lander]
  (next-trace lander)
  (let [G (future (make-guide scape lander))]
    (loop [g (deref G 128 nil)]
      (if g 
        (do (next-guide g))
        (do (trace-move control)
                  (recur (deref G 128 nil)))))))

(defn- guide-loop [^geometry.Landscape scaple
                   ^lander.Lander lander
                   guide])

(comment (defn- approximate-move [^lander.Control control trace]
           (let [l (move control 1.0 ^Lander (last trace))]
             (conj trace (assoc l :x (Math/round ^double (:x l))
                                :y (Math/round ^double (:y l))
                                :vx (Math/round ^double (:vx l))
                                :vy (Math/round ^double (:vy l))))))

         (defn- trace-move [^lander.Control control trace]
           (conj trace (move control 1.0 ^Lander (last trace))))

         (defn- approximate-last [trace]
           (let [l ^Lander (last trace)]
             (assoc l :x (Math/round ^double (:x l))
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
        L (:lander (test-data 0))
        S (detect-landscape (:surface (test-data 0)))
        G (future (make-guide L S))]
    (dump "surface: " S)
    (dump "lander: " L)

    (let [[guide trace-04] (loop [t [(form-lander L)] g (deref G 128 nil)]
                             (if g
                               [g t]
                               (recur (trace-move (->Control 0 4) t)
                                      (deref G 128 nil))))]
      (dump "guide-length:" (count guide))
      (dump "trace:" trace-04)
      (r/update-scene :lander-traces [trace-04])
      
      (let [trace (loop [t [(last trace-04)]]
                    (let [control (along-guide (approximate-last t) guide)]
                      (if (nil? control)
                        t
                        (recur (trace-move control t)))))]
        (r/update-scene :lander-traces [trace-04 trace])
        (last trace)))))

; eval

(comment (-main) 
         (def ^:private ^:const bad-cases [{:C (->Control 60 4)
                                            :S (nth stages 2) 
                                            :L #lander.Lander{:x 1500.0, :y 2514.4499999999994, :vx 100.0, :vy -37.10999999999999, :fuel 800,
                                                              :control #lander.Control{:angle -15, :power 0}}}

                                           {:C (->Control 20 4)
                                            :S (nth stages 2)
                                            :L #lander.Lander{:x 1500.3393543299987, :y 2529.3060059296076, :vx 99.64872884586892, :vy -27.17071128706871, :fuel 790,
                                                              :control #lander.Control{:angle 5, :power 4}}}])
         (r/sketch-up)
         

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
