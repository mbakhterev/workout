(ns Player (:gen-class))

(defn- dump [& args] (binding [*out* *err*] (apply println args)))

(def ^:const ^:private move-left [-1 0])
(def ^:const ^:private move-right [1 0])
(def ^:const ^:private move-down [0 1])

(def ^:const ^:private table [{} ;0
                              {'TOP move-down 'LEFT move-down 'RIGHT move-down} ; 1
                              {'LEFT move-right 'RIGHT move-left} ; 2
                              {'TOP move-down} ; 3
                              {'TOP move-left 'RIGHT move-down}
                              {'TOP move-right 'LEFT move-down} ; 4
                              {'LEFT move-right 'RIGHT move-left} ; 5
                              {'TOP move-down 'RIGHT move-down} ; 6
                              {'LEFT move-down 'RIGHT move-down} ; 7
                              {'LEFT move-down 'TOP move-down} ; 8
                              {'TOP move-left} ; 9
                              {'TOP move-right} ; 10
                              {'RIGHT move-down} ; 11
                              {'LEFT move-down}]) ; 12

(defn -main [& args]
  (let [W (read)
        H (read)
        M (mapv vec (partition W (repeatedly (* W H) read)))
        X (read)]
    (doseq [l M] (dump l))
    (loop [x (read) y (read) entry (read)]
      (let [tile (get-in M [y x])
            [dx dy] (get-in table [tile entry])]
        (dump x y "tile:" tile (get-in table [tile]) entry (type entry) "→" dx dy)
        (println (+ x dx) (+ y dy))
        (recur (read) (read) (read))))))
