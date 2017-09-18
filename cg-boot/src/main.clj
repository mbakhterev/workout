(ns main (:gen-class)
         (:require [lander :as l]
                   [geometry :as g]
                   [render :as r]))

(set! *warn-on-reflection* true)

(defn- dump [& args] (binding [*out* *err*] (apply println args)))
(defn- read-surface [] (let [N (read)] (doall (repeatedly (* 2 N) read))))
(defn- read-lander [] (doall (repeatedly 7 read)))

(comment (defn- refine-guide [guide] (vec (apply concat (first guide) (map rest (rest guide))))))

(comment (defn- refine-guide [guide] (vec (apply concat guide))))

(defn- sketch-landscape [^geometry.Landscape scape]
  (r/update-scene :surface (:raw-surface scape))
  (r/update-scene :landing-pad (:landing-pad scape)) 
  (r/update-scene :shell (concat (:left-rock scape)
                                 [(:landing-pad scape)]
                                 (:right-rock scape))))

(let [state (atom {:guides [] :traces [] :stages []})]
  (defn- reset-state []
    (reset! state {:guides [] :traces [] :stages []})
    (r/update-scene :guide-traces nil)
    (r/update-scene :lander-traces nil)
    (r/update-scene :stages nil)) 

  (defn- next-guide [g]
    (swap! state (fn [st] (assoc st :guides (concat (:guides st) g))))) 

  (defn- next-trace [^lander.Lander l]
    (swap! state (fn [st] (assoc st :traces (conj (:traces st) [l])))))

  (defn- next-stages [s]
    (swap! state (fn [st] (assoc st :stages (conj (:stages st) s)))))

  (comment (defn- unpack-traces [t]
             (and (not (empty? t))
                  (not (empty? (last t)))
                  (= lander.Lander (type (last (last t))))
                  ;[(drop-last t) (last t)]
                  [(vec (drop-last t)) (vec (last t))]
                  )))

  (defn- unpack-traces [t]
    (if (and (not (empty? t))
             (not (empty? (last t)))
             (= lander.Lander (type (last (last t)))))
      ; [(drop-last t) (last t)]
      [(vec (drop-last t)) (vec (last t))]))

  (defn- sketch-state []
    (let [st (deref state)]
      (if (unpack-traces (:guides st)) (r/update-scene :guide-traces (:guides st)))
      (if (unpack-traces (:traces st)) (r/update-scene :lander-traces (:traces st)))
      (if (not (empty? (:stages st))) (r/update-scene :stages (first (:stages st))))))

  (defn- trace-move [^lander.Control control]
    (swap! state (fn [st] (let [[prev curr :as traces] (unpack-traces (:traces st))]
                            (assert traces)
                            (assoc st :traces
                                   (conj prev (conj curr (l/move control 1.0 ^Lander (last curr)))))))))
  
  (defn- approximate-last []
    (let [[prev curr :as traces] (unpack-traces (:traces (deref state)))]
      (assert traces)
      (let [l ^lander.Lander (last curr)]
        (assoc l :x (Math/round ^double (:x l))
                 :y (Math/round ^double (:y l))
                 :vx (Math/round ^double (:vx l))
                 :vy (Math/round ^double (:vy l)))))))

(comment (defn- make-guide [^lander.Lander {x :x vx :vx :as i-lander}
                            ^geometry.Landscape scape]
           (let [stages (detect-stages x vx scape)]
             (next-stages stages)
             (sketch-state)
             (l/debugln :make-guide stages)
             (let [guide (model-control (l/search-guide stages i-lander) i-lander)]
               (next-guide guide)
               (sketch-state)
               (refine-guide guide)))))

(defn- make-guide [^lander.Lander {x :x vx :vx :as i-lander}
                            ^geometry.Landscape scape]
           (let [stages (g/detect-stages x vx scape)]
             (next-stages stages)
             (sketch-state)
             (l/debugln :make-guide stages)
             (let [guide (l/search-guide stages i-lander)]
               (next-guide guide)
               (sketch-state)
               (l/refine-guide guide))))

(def ^:private ^:const quanta 128)

(defn- wait-loop [^geometry.Landscape scape
                  ^lander.Control control
                  ^lander.Lander lander]
  (next-trace lander)
  (loop [l lander G (future (make-guide lander scape)) steps 0]
    (if-let [g (deref G quanta nil)] 
      (do (dump "guide computation is DONE." "steps:" steps "guide length:" (count g))
          (if (empty? g)
            (do (dump "guide computation is FAILED. Continuing with routine")
                (trace-move control)
                (let [tl (approximate-last)]
                  (recur tl (future (make-guide tl scape)) 0)))
            [l g]))
      (do (dump "waiting for guide with control:" control)
          (trace-move control)
          (recur (approximate-last) G (+ 1 steps))))))

(def ^:private ^:const tolerable-drift (* 4.0 4.0))

(defn- guide-loop [^lander.Lander lander guide]
  (next-trace lander)
  (loop [l lander steps 0]
    (if-let [[delta control] (l/along-guide l guide)]
      (if (> delta tolerable-drift)
        (do (dump "guide drift is too large. Correction is needed."
                  "delta:" delta
                  "steps:" steps
                  "x:" (:x (approximate-last))
                  "lander:" l)
            l)
        (do (dump "delta is ok:" delta)
            (trace-move control)
            (recur (approximate-last) (+ 1 steps))))
      (do (dump "guide following is DONE")
          (sketch-state)
          nil))))

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
                                   :lander [6500 2700 -50 0 1000 90 0]}
                                  
                                  {:surface [0 100 1000 500 1500 1500 3000 1000 4000 150 5500 150 6999 800]
                                   :lander [6500 2800 -90 0 750 90 0]}])

(comment (let [T (test-data 1)
               S (g/build-landscape (:surface T))
               bad-lander #lander.Lander{:x 4774.0, :y 2521.0, :vx -60.0, :vy -4.0, :fuel 891, :control #lander.Control{:angle 0, :power 4}} 
               L (if true bad-lander (l/form-lander (:lander T)))
               stages (detect-stages (:x L) (:vx L) S)
               guide (search-guide stages L)
               control (model-control guide L)]
           (reset-state)
           (sketch-landscape S)
           (next-stages stages)
           (next-guide control)
           (sketch-state)
           ; (r/update-scene :guide-traces control)
           ; (r/update-scene :lander-traces (list (refine-guide control)))
           ; (println (map type guide) \newline)
           ; (println guide \newline)
           ; (println control \newline)
           ; (doseq [c control] (println (type c) (count c)))
           ; (refine-guide control)
           (make-guide L S)
           (along-guide L (refine-guide control))
           ; (map type (refine-guide control))
           ; (second (refine-guide control))
           ; (map type (refine-guide control))
           ; (take 2 (refine-guide control))
           ))  


(comment (let [t (test-data 1)
               s (g/build-landscape (:surface t))
               l (form-lander (:lander t))
               stages (detect-stages (:x l) (:vx l) s)
               guide (search-guide stages l)]
           (reset-state)
           (sketch-landscape s)
           (next-stages stages)
           (next-guide guide)
           (sketch-state)
           (map type guide)))


(defn -main [& args]
  (reset-state)
  (let [T (test-data 1)
        S (g/build-landscape (:surface T))
        L (l/form-lander (:lander T))]
    (sketch-landscape S)
    (loop [l L]
      (let [[lw g] (wait-loop S (l/->Control 0 4) l)]
        (dump "guide length:" (count g))
        (if-let [lg (guide-loop lw g)]
          (recur lg)
          (approximate-last)))))) 

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
         (map (juxt :stage :x-goal) stages)
         (intersect-time  [0.1 10 0] [-0.3 0 0] [0 -21 14 -1])
         (do (r/sketch-up) (main/-main)))

(defn bad-test []
  (let [bad-l
        #lander.Lander{:x 1751.0, :y 2522.0, :vx 108.0, :vy -21.0, :fuel 774,
                       :control #lander.Control{:angle -5, :power 3}}
        scape (g/build-landscape (:surface (test-data 0)))
        stages (g/detect-stages bad-l scape)
        guide (make-guide bad-l scape)]
    (map :stage stages)
    (l/search-guide stages bad-l))) 

(defn bad-test-2 []
  (let [T (test-data 2)
        L (l/form-lander (:lander T))
        S (g/build-landscape (:surface T))
        stages (g/detect-stages L S)]
    L))
