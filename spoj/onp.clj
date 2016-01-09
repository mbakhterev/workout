(ns spoj.onp)

(set! *warn-on-reflection* true)

(def L {\+ 0 \- 0 \* 1 \/ 1 \^ 2 \( -2 \) 3})
(def R {\+ 0 \- 0 \* 1 \/ 1 \^ 2 \( 3 \) -1})
(def prio-max (+ 1 (apply max (concat (vals L) (vals R)))))

(defn- prio [T c] (if-let [p (T c)] p prio-max))

(defn- cmp [op l r] (op (prio L l) (prio R r)))

(defn- shift [prev-state c]
  (let [A (:accum prev-state) 
        S (:state prev-state)
        [s state] (split-with #(cmp >= % c) S)]
    {:accum (concat A s) :state (if (= c \)) (next state) (cons c state))}))

(defn- push [S c] {:accum (:accum S) :state (cons c (:state S))})

(defn- add-one [S c]
; (print S "\n")
  (let [top (-> S :state first)]
    (if (cmp < top c) (push S c) (shift S c))))

(def empty-state {:accum '() :state '(\()})

(defn- process-one [expr]
  (clojure.string/join ""
    (:accum (shift (reduce add-one empty-state expr) \)))))

(defn- run-one [] (let [expr (read-line)] (print (process-one expr) "\n")))

(defn- run [] (dorun (repeatedly (read-string (read-line)) run-one)))

(run)

; (clojure.string/join ""
;   (:accum
;     (shift (reduce add-one (empty-state) "((a+t)*((b+(a+c))^(c+d)))") \))))
; 
; (clojure.string/join ""
;   (:accum
;     (shift (reduce add-one (empty-state) "(a+t)*(c+d)") \))))
; 
; (clojure.string/join ""
;   (:accum (shift (reduce add-one {:accum '() :state '(\$)} "a+b*c^d*e-f") \$)))
; 
; (lt \+ \b)
; 
; (:state {:accum '() :state 1})
