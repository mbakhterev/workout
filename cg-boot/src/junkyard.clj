(comment (let [T (test-data 1)
               S (g/build-landscape (:surface T))
               bad-lander #lander.Lander{:x 4774.0, :y 2521.0, :vx -60.0, :vy -4.0, :fuel 891, :control #lander.Control{:angle 0, :power 4}} 
               L (if true bad-lander (l/lander (:lander T)))
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
        S (g/landscape (:surface T))
        stages (g/detect-stages L S)]
    L))

(defn- extract-bad-lander []
  (let [T (test-data 1)
        S (g/make-landscape (:surface T))
        L (l/make-lander (:lander T))
        LP (:landing-pad S)
        bad-lander (fn [^lander.Lander l] (< (:y l) (:ay LP)))
        ]
    (main/-main)
    (println "LP:" LP)
    (let [bad-guides (filter (comp bad-lander last last) (keep identity (state-guides)))
          the-worst (first (sort-by (comp :y last last) bad-guides))]
      (r/update-scene :guides (list the-worst))
      (first (first the-worst)))))

(def ^:const ^:private bad-lander (extract-bad-lander))

(defn run-bad-case []
  (let [T (test-data 1)
        L bad-lander 
        S (g/make-landscape (:surface T))
        stages (g/make-stages (:x L) (:vx L) S)
        guide (l/search-guide stages L)]
    (reset-state)
    (next-stages stages)
    (next-guide guide)
    (sketch-state)
    (println "LP:" (:landing-pad S))
    (println "last stage:" (:stage (last stages)) "section:" (:section (last stages))))) 
