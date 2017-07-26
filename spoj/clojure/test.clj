(ns spoj.test)

(defn read-int [] (if-let [str (read-line)] (Integer/parseInt str)))

; Очевидный цикл
(defn simple-loop []
  (loop [] (if-let [x (read-int)] (when (not= 42 x) (println x) (recur)))))

; Ленивая последовательность
(defn lazy-loop []
  (doall
    (map println
      ((fn it []
         (lazy-seq (if-let [x (read-int)] (if (not= x 42) (cons x (it))))))))))

(defn read-lines [] (line-seq (java.io.BufferedReader. *in*)))
 
(defn run-loop []
  (->> (read-lines) (take-while (partial not= "42")) (map println) dorun))

(run-loop)
