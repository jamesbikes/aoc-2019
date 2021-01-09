(ns aoc.day03
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as set]))

(def dxdy {
  "U" [0, 1],
  "D" [0, -1],
  "L" [-1, 0],
  "R" [1, 0]})

(defn update-grid [grid x y n]
  (if (contains? grid [x y])
    grid
    (assoc grid [x y] n)))

(defn manhattan [a]
  (reduce + (map #(Math/abs %) a)))

(defn parseInstr [instr]
  (let [[_ dir n] instr]
    (vector dir (Integer/parseInt n))))

(defn parse [data]
  (->> data
    (re-seq #"(.)(\d+)")
    (map parseInstr)))

(defn apply-element [grid x y steps element]
  (let [grid (update-grid grid x y steps)
        [dir n] element
        [dx dy] (dxdy dir)]
    (if (== 0 n)
      [grid x y steps]
      (recur grid (+ x dx) (+ y dy) (inc steps) [dir (dec n)]))))

(defn visit [path]
  (loop [grid {}
         x 0
         y 0
         steps 0
         [p & ps] path]
    (if (nil? p)
      grid
      (let [[grid x y steps] (apply-element grid x y steps p)]
        (recur grid x y steps ps)))))


(def data 
  (->> (slurp "data/day03b.txt")
    str/split-lines
    (map parse)))

(defn part2 [a b]
  (let [common (set/intersection (set (keys a)) (set (keys b)))
        steps (into {} (map (fn [k] [k (+ (a k) (b k))]) common))]
    (min-key steps common)))

; part 1
(->> data
  (map visit)
  (map keys)
  (map set)
  (apply set/intersection)
  (map manhattan)
  (apply sorted-set)
  second
)

; part 2
(->> data
  (map visit)
  (apply part2))
  