(ns Player (:gen-class))

; Auto-generated code below aims at helping you parse
; the standard input according to the problem statement.

(defn -main [& args]
  (let [[n-floors
         width
         n-rounds
         exit-floor
         exit-pos
         n-total-clones
         n-additional-elevators
         n-elevators :as setup] (repeatedly 8 read)
        ; n-floors: number of floors
        ; width: width of the area
        ; n-rounds: maximum number of rounds
        ; exit-floor: floor on which the exit is found
        ; exit-pos: position of the exit on its floor
        ; n-total-clones: number of generated clones
        ; n-additional-elevators: ignore (always zero)
        ; n-elevators: number of elevators

        elevators (repeatedly (* 2 n-elevators) read)]
    (loop [[clone-floor
            clone-pos
            clone-direction] (repeatedly 3 read)]
      ; clone-floor: floor of the leading clone
      ; clone-pos: position of the leading clone on its floor
      ; direction: direction of the leading clone: LEFT or RIGHT

      (binding [*out* *err*]
        (println "setup:" setup "setup-len:" (count setup))
        (println "n-elevators:" n-elevators "elevators:" elevators)
        (println "clone:" clone-floor clone-pos clone-direction))

      ; action: WAIT or BLOCK
      (println "WAIT")
      
      (recur (repeatedly 3 read)))))

; (defn -main [& args] (binding [*out* *err*] (while true (println (read-line)))))