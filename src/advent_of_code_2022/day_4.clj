(ns advent-of-code-2022.day-4
  (:require [clojure.string :as str]
            [advent-of-code-2021.util :as util]))


(defn parse [line]
  (->> line
       (re-matches #"(\d*)-(\d*),(\d*)-(\d*)")
       rest
       (map util/to-int)
       (partition 2)))

(defn input []
  (->> (slurp "resources/2022/day_4")
       str/split-lines
       (map parse)))

(defn subset? [[a b] [x y]]
  (and (<= a x)
       (>= b y)))

(defn overlaps? [[a b] [x y]]
  (or (<= x a y)
      (<= x b y)))

(defn redundant? [comp-fn]
  (fn [[a b]]
    (or (comp-fn a b) (comp-fn b a))))

(defn pt1 []
  (->> (input)
       (filter (redundant? subset?))
       count))

(defn pt2 []
  (->> (input)
       (filter (redundant? overlaps?))
       count))
