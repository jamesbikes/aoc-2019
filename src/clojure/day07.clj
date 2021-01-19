(ns day07
    (:require [clojure.math.combinatorics :as combo])
    (:require [clojure.core.async :as async :refer [>! >!! <!! chan thread]]))
  
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
  {:memory (assoc memory a (<!! input))
   :pc (+ pc 2)})

(defn op-output [[a] [mode] {:keys [memory pc output]}]
  (let [x (resolve-param a mode memory)]
    (do (>!! output x)
      {:pc (+ pc 2)})))

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

(defn run-it [memory input output]
  (loop [state {:pc 0, :memory memory, :input input, :output output, :halt false}]
    (if (state :halt) nil (recur (process-op state)))))
  
(defn parse [r]
  (->> r
    (re-seq #"-?\d+")
    (map #(Integer/parseInt %))
    (apply vector)))

; phase 1 amp
(defn make-amp [memory phases]
  (let [input (chan 1)]
    (loop [cur-in input, [x & xs] phases]
      (let [cur-out (chan 1)
        t (thread (run-it memory cur-in cur-out))]
          (>!! cur-in x)
          (if xs
            (recur cur-out xs)
            [input cur-out t])))))

(defn amp-it [memory phases]
  (let [[in out _] (make-amp memory phases)]
    (do
      (>!! in 0)
      (<!! out))))
  
; part 1
(defn part1 [data]
  (let [memory (parse data)]
    (->> [0 1 2 3 4]
        combo/permutations
        (map #(amp-it memory %))
        (apply max))))

(part1 (slurp "data/day07.txt"))


; phase 2
(defn make-amp2 [memory phases]
  (let [channels (repeatedly (count phases) #(chan 1))]
    (loop [[x & xs] phases
           [c1 c2 & cs] channels]
      (let [in c1
            out (or c2 (first channels))
            t (thread (run-it memory in out))
            _ (>!! in x)]
        (if xs
          (recur xs (cons c2 cs)) [out t])))))

(defn amp-it2 [memory phases]
  (let [[inout done] (make-amp2 memory phases)]
    (do
      (>!! inout 0)
      (<!! done)      ; waits for channel to close, last amp has shut down
      (<!! inout))))

(defn part2 [data]
  (let [memory (parse data)]
    (->> [5 6 7 8 9]
        combo/permutations
        (map #(amp-it2 memory %))
        (apply max))))

(part2 (slurp "data/day07.txt"))
