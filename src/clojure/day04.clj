(ns aoc.day03
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as set]))

(defn digits [n]
    (if (pos? n)
        (conj (digits (quot n 10)) (mod n 10) )
        []))

(defn valid [n]
  (let [d (digits n)]
    (and
        (= 6 (count d))
        (some true? (map = (rest d) d))       ; two sequential numbers are same
        (every? true? (map >= (rest d) d))))) ; all numbers ascending

(defn valid2 [n]
    (let [d (digits n)]
        (and
            (= 6 (count d))
            (some #(= 2 (count %)) (partition-by identity d))
            (every? true? (map >= (rest d) d)))))

(defn part1 [start stop]
    (count (filter valid (range start stop))))


(defn part2 [start stop]
    (count (filter valid2 (range start stop))))
    
(part1 158126 624574)
(part2 158126 624574)
