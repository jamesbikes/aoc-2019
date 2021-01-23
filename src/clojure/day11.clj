(ns day11
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

(defn resolve-input [x mode {:keys [memory base]}]
  (case mode
    0 (get memory x 0)
    1 x
    2 (get memory (+ base x) 0)))

(defn resolve-output [x mode {:keys [memory base]}]
  (case mode
    0 x
    2 (+ base x))) ; mode '1' is not allowed, will throw

(defn binary-op [f args modes {:keys [memory pc] :as state}]
  (let [a (resolve-input (first args) (first modes) state)
        b (resolve-input (second args) (second modes) state)
        c (resolve-output (nth args 2) (nth modes 2) state)
        result (f a b)]
    {:memory (assoc memory c result), :pc (+ pc 4)}))

(defn op-input [[a] [mode] {:keys [memory pc input], :as state}]
  {:memory (assoc memory (resolve-output a mode state) (<!! input))
   :pc (+ pc 2)})

(defn op-output [[a] [mode] {:keys [pc output] :as state}]
  (let [x (resolve-input a mode state)
        _ (>!! output x)]
    {:pc (+ pc 2)}))

(defn jump-op [f args modes {:keys [pc] :as state}]
  (let [input (resolve-input (first args) (first modes) state)
        target (resolve-input (second args) (second modes) state)]
    {:pc (if (f input) target (+ pc 3))}))

(defn op-base [[a] [mode] {:keys [base pc] :as state}]
  (let [x (resolve-input a mode state)]
    {:base (+ base x)
     :pc (+ pc 2)}))

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
  9 op-base
  99 op-halt})

(defn process-op [state] 
  (let [{:keys [memory pc]} state
    instr (memory pc)
    args (map #(memory (+ pc %)) [1 2 3])
    [opcode modes] (parse-instruction instr)
    f (ops opcode)]
    (merge state (f args modes state))))

(defn run-it [memory input output]
  (loop [state {:pc 0, :memory memory, :input input, :output output, :halt false, :base 0}]
    (if (state :halt) (async/close! output) (recur (process-op state)))))

(defn to-hash [xs]
  (into {} (map-indexed vector xs)))

(defn parse [r]
  (->> r
    (re-seq #"-?\d+")
    (map #(read-string %))
    to-hash))

; 0 = north, 1 = east, 2 = south, 3 = west
(def dxdy [[0 1] [1 0] [0,-1] [-1 0]])

; return new direction, given input: 0 = left, 1 = right
(defn rotate [dir input]
  (mod (case input 0 (dec dir), 1 (inc dir) dir) 4))

(defn move [pos dir]
  (vec (map + pos (dxdy dir))))

(def memory (parse (slurp "data/day11.txt")))

(defn paint-it [memory init-val]
  (let [input (chan 1)
        output (chan 2)
        _ (thread (run-it memory input output))]
    (loop [grid {[0 0] init-val}, pos [0 0], dir 0]
        (let [_ (>!! input (get grid pos 0))
                color (<!! output)
                turn (<!! output)
                new-dir (rotate dir turn)]
        (if color
            (recur (assoc grid pos color) (move pos new-dir) new-dir)
            grid)))))

; part 1
(count (paint-it memory 0))
