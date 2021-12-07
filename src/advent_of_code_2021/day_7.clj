(ns advent-of-code-2021.day-7
  (:require [clojure.string :as str]
            [advent-of-code-2021.util :as util]))

(defn input
  []
  (->> (slurp "resources/day_7")
       util/commas->ints))

(defn constant-cost
  [pos crab]
  (Math/abs ^long (- crab pos)))

(def inc-cost
  (memoize
    (fn [pos crab]
      (->> (Math/abs ^long (- crab pos))
           inc
           (range)
           (reduce + 0)))))

(defn position-cost
  [crabs cost-fn]
  (reduce + (map cost-fn crabs)))

(defn max-pos
  [crabs]
  (reduce max crabs))

(defn min-cost
  [crabs cost-fn]
  (reduce min (map #(position-cost crabs (partial cost-fn %)) (range (max-pos crabs)))))

(defn pt1
  []
  (min-cost (input) constant-cost))

(defn pt2
  []
  (min-cost (input) inc-cost))