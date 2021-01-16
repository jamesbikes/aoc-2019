(defn digits [n]
    (if (pos? n)
        (conj (digits (quot n 10)) (mod n 10) )
        []))

(defn parse-instruction [x]
  (let [opcode (mod x 100)
        modes (quot x 100)]
    (as-> modes $
        (digits $)
        (reverse $)
        (take 3 (concat $ (repeat 0))) ; makes 3 element list, padded with 0s
        [opcode $])))

(defn resolve-param [x mode memory] (if (= 0 mode) (memory x) x))

(defn binary-op [f args modes {:keys [memory pc]}]
  (let [a (resolve-param (first args) (first modes) memory)
        b (resolve-param (second args) (second modes) memory)
        result (f a b)]
    {:memory (assoc memory (nth args 2) result), :pc (+ pc 4)}))

(defn op-input [[a] _ {:keys [memory pc input]}]
  {:memory (assoc memory a input), :pc (+ pc 2)})

(defn op-output [[a] [mode] {:keys [memory pc]}]
  (let [output (resolve-param a mode memory)]
    {:output (memory a), :pc (+ pc 2)}))

(defn jump-op [f args modes {:keys [memory pc]}]
  (let [input (resolve-param (first args) (first modes) memory)
        target (resolve-param (second args) (second modes) memory)]
    {:pc (if (f input) target (+ pc 3))}))

(defn op-halt [& _] {:halt true})

(def ops {
  1 (partial binary-op +)
  2 (partial binary-op *)
  3 op-input
  4 op-output
  5 (partial jump-op #(not (zero? %)))
  6 (partial jump-op zero?)
  7 (partial binary-op #(if (< %1 %2) 1 0))
  8 (partial binary-op #(if (= %1 %2) 1 0))
  99 op-halt})

(defn process-op [state] 
  (let [{:keys [memory pc]} state
    [instr & args] (subvec memory pc)
    [opcode modes] (parse-instruction instr)
    f (ops opcode)]
    (merge state (f args modes state))))

(defn run-it [memory input]
  (loop [state {:pc 0, :memory memory, :input input, :halt false}]
    (if (state :halt) (state :output) (recur (process-op state)))))   
      
(defn parse [r]
  (->> r
    (re-seq #"-?\d+")
    (map #(Integer/parseInt %))
    (apply vector)))
 
; part 1
(run-it (parse (slurp "data/day05.txt")) 1)

; part 2
(run-it (parse (slurp "data/day05.txt")) 5)

