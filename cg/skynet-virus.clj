(ns Player (:gen-class))

; Auto-generated code below aims at helping you parse
; the standard input according to the problem statement.

(defn- trace [G I X]
  (loop [Q X V (set X)]
    (if (empty? Q)
      [0 1]
      (let [q (first Q)
            e (filter (comp not V) (G q))]
        (if (some (partial = I) e)
          [q I]
          (recur (concat (rest Q) e) (conj V q)))))))

(defn -main [& args]
  (let [[N L E] (repeatedly 3 read)
        G (reduce (fn [R [n m]] (assoc R
                                       n (conj (R n #{}) m)
                                       m (conj (R m #{}) n)))
                  {}
                  (repeatedly L (fn [] [(read) (read)])))
        X (repeatedly E read)]
    ; N: the total number of nodes in the level, including the gateways
    ; L: the number of links
    ; E: the number of exit gateways
    ; G: network graph
    ; X: exit nodes

    (binding [*out* *err*] (println G \newline X))

    (loop [Net G]
      (let [SI (read)
            [a b] (trace Net SI X)]
        ; SI: The index of the node on which the Skynet
        ; agent is positioned this turn

        (comment (binding [*out* *err*] (println SI a b)))

        ; Example: 0 1 are the indices of the nodes you wish
        ; to sever the link between
        (println a b)
        (recur (assoc Net a (disj (Net a #{}) b) b (disj (Net b #{}) a)))))))
