(ns Solution (:gen-class))

; Auto-generated code below aims at helping you parse
; the standard input according to the problem statement.

(defn- binarize [c] (rest (Integer/toBinaryString (bit-or 0x80 c))))

(defn- translate [C]
  (concat (condp = (first C) \1 '(\0) \0 '(\0 \0)) '(\space) (repeat (count C) \0) '(\space)))

(defn -main [& args]
  (let [M (read-line)
        B (mapcat (comp binarize int) M)
        T (mapcat translate (partition-by identity B))]

    (binding [*out* *err*]
      (println B (count B) (apply str T)))

    ; Write answer to stdout
    (println (apply str (drop-last T)))))
