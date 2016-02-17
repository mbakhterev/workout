(ns Solution (:gen-class))

; Auto-generated code below aims at helping you parse
; the standard input according to the problem statement.

(defn -main [& args]
  (let [H (sort (repeatedly (read) read))
        A (apply min (map (comp #(Math/abs %) (partial apply -)) (partition 2 1 H)))]

    (comment (binding [*out* *err*] (println H A)))

    ; Write answer to stdout
    (println A)))
