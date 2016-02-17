(ns Solution
  (:gen-class))

; Auto-generated code below aims at helping you parse
; the standard input according to the problem statement.

(defn -main [& args]
  (let [N (read)
        Q (read)
        M (into {} (repeatedly N (fn [] [(str "." (clojure.string/lower-case (read))) (read)])))
        ext-re (re-pattern #"\.\w+$")
        ext (comp last (partial re-seq ext-re) clojure.string/lower-case)
        RQ (repeatedly Q (comp str read))]

    (comment (binding [*out* *err*]
               (println (map (comp (fn [e] (M e "UNKNOWN")) ext) RQ))
               (println M)
               (println RQ)
               (println (map ext RQ))))

    ; For each of the Q filenames, display on a line the corresponding
    ; MIME type. If there is no corresponding type, then display UNKNOWN.
    (dorun (map (comp println (fn [e] (M e "UNKNOWN")) ext) RQ))))
