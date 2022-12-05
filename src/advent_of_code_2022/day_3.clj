(ns advent-of-code-2022.day-3
  (:require [clojure.string :as str]
            [advent-of-code-2021.util :as util]
            [clojure.set :as set]))

(defn input []
  (->> (slurp "resources/2022/day_3")
       str/split-lines
       (map identity)
       (map #(split-at (/ (count %) 2) %))))

(defn find-common
  [[a b]]
  (first (set/intersection (into #{} a) (into #{} b))))

(defn char->priority
  [char]
  (let [i (int char)]
    (cond (> i 91) (- i 96)
          :else (- i 38))))

(defn pt1
  []
  (->> (input)
       (map find-common)
       (map char->priority)
       (reduce +)))

(defn pt2-input []
  (->> (slurp "resources/2022/day_3")
       str/split-lines
       (map identity)
       (partition 3)))

(defn pt2-common [seq]
  (first (apply set/intersection (map #(into #{} %) seq))))

(defn pt2
  []
  (->> (pt2-input)
       (map pt2-common)
       (map char->priority)
       (reduce +)))