(ns day12
  (:require [clojure.math.combinatorics :as c])
  (:require [clojure.math.numeric-tower :as math]))

(defn parse-int [x] (Integer/parseInt x))

; planets passed around as [position velocity] vectors
; where position = [x y z] and velocity = [x y z]
; p = position, v = velocity

(defn gravity [[pa va] [pb vb]]
  (let [deltas (map compare pa pb)] ; 'compare' returns -1, 0, or 1
    [[pa (mapv - va deltas)] [pb (mapv + vb deltas)]]))

(defn velocity [[p v]]
  [(mapv + p v) v])

(defn energy [x]
  (reduce + (map #(Math/abs %) x)))

(defn total-energy [[p v]]
  (* (energy p) (energy v)))

(defn apply-gravity [planets]
  (reduce (fn [xs [i j]]
    (let [[xi xj] (gravity (xs i) (xs j))]
      (assoc xs, i xi, j xj)))
    planets
    (c/combinations (range (count planets)) 2)))

(defn apply-velocity [planets]
  (mapv velocity planets))

(defn step 
  ([planets]
    (->> planets
      apply-gravity
      apply-velocity))

  ; steps n times.
  ; of course, there's no 'for' loop in clojure and 'dotimes' doesn't do what I want it to do, so 'reduce' it is...  
  ([planets n]
    (reduce (fn [x _] (step x)) planets (range n))))

(defn parse [data]
  (->> data
    (re-seq #"<x=(-?\d+), y=(-?\d+), z=(-?\d+)>")
    (map rest)
    (map #(mapv parse-int %))))

(defn part1 [input]
  (as-> input $
    (parse $)
    (mapv #(vector % [0 0 0]) $)
    (step $ 1000)
    (map total-energy $)
    (reduce + $)))

(defn search [state prevs]
  (let [n (count prevs)]
    (if-let [first (prevs state)]
      n
      (recur (step state) (assoc prevs state n)))))

(defn find-cycle [input i]
  (as-> input $
    (map #(nth % i) $)
    (mapv #(vector [%] [0]) $)
    (search $ {})))

(defn part2 [input]
  (as-> input $
    (parse $)
    (map #(find-cycle $ %) [0 1 2])
    (reduce math/lcm $)))

(def input (slurp "data/day12.txt"))

(println (part1 input))
(println (part2 input))
