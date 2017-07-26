(ns spoj.prime1)

(set! *unchecked-math* true)
(set! *warn-on-reflection* true)

(defn- sieve-bound [N] (+ 1 (long (Math/sqrt N))))

(defn- is-prime [P n] (not (some #(and (zero? (mod n %)) (not= n %)) P)))

(defn- prime-reduce [P n] (if (is-prime P n) (conj P n) P))

(defn- prime-sieve [N] (reduce prime-reduce [2] (range 3 N 2)))

(defmacro prime-gen [N] (vec (prime-sieve (eval N))))

(def prime-vec (prime-gen (sieve-bound 1000000000)))

(defn- next-odd [n] (if (even? n) (+ n 1) n))

(defn- sieve-range [M N]
  (let [P (vec (take-while (partial >= (+ 1 (Math/sqrt N))) (rest prime-vec)))
        R (range (next-odd (max M 2)) (inc N) 2)]
    (filter (partial is-prime P) R)))

(defn- to-int [s] (Integer/parseInt s))

(defn- process-pair []
  (let [[M N] (map to-int (clojure.string/split (read-line) #" "))]
    (print (clojure.string/join \newline (sieve-range M N)) "\n\n")))

(defn- run [] (dorun (repeatedly (to-int (read-line)) process-pair)))

(run)
