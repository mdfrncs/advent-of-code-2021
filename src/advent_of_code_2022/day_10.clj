(ns advent-of-code-2022.day-10
  (:require [clojure.string :as str]
            [advent-of-code-2021.util :as util]))

(defn parse-line [line]
  (if (= line "noop")
    [:noop]
    [:addx (->> line (re-matches #"addx (-*\d+)") last util/to-int)]))

(defn input []
  (->> (slurp "resources/2022/day_10")
       str/split-lines
       (map parse-line)))

(defn commands->deltas [input]
  (mapcat (fn [[i x]]
            (if (= i :addx)
              [0 x]
              [0]))
          input))

(defn deltas->x [flat]
  (reduce (fn [xs i]
            (conj xs (+ i (last xs))))
          [1]
          flat))

(defn pt1 []
  (let [xs (->> (input) commands->deltas deltas->x)]
    (->> [20 60 100 140 180 220]
         (map #(* (nth xs (dec %)) %))
         (reduce +))))

;; PT2

(defn draw [idx x]
  (let [ri (mod idx 40)]
    (when (= 0 ri) (println))
    (if (or (= x ri) (= (dec x) ri) (= (inc x) ri))
      (print "#")
      (print "."))))

(defn pt2 []
  (let [xs (->> (input) commands->deltas deltas->x)]
    (doall (map-indexed draw xs))
    nil))