(ns advent-of-code-2021.day-13
  (:require [clojure.string :as str]))

(defn ->int
  [str]
  (Integer/valueOf ^String str))

(defn parse
  [lines]
  (reduce (fn [acc line]
            (let [[_ dir idx] (re-matches #"fold along ([xy])=(\d+)" line)
                  [_ x y] (re-matches #"(\d+),(\d+)" line)]
              (cond
                (empty? line) acc
                idx (update acc :folds conj [(keyword dir) (->int idx)])
                :else (update acc :points conj {:x (->int x)
                                                :y (->int y)}))))
          {:folds  []
           :points #{}}
          lines))

(defn in
  [file]
  (->> (slurp file)
       str/split-lines
       parse))

(defn input [] (in "resources/day_13"))
(defn example [] (in "resources/day_13_example"))

(defn reflect
  [idx v]
  (if (< idx v) (- idx (- v idx)) v))

(defn fold
  [points [dir idx]]
  (->> points
       (map #(update % dir (partial reflect idx)))
       (into #{})))

(defn pt1
  []
  (let [{:keys [folds points]} (input)]
    (count (fold points (first folds)))))

(defn max-pos
  [dir points]
  (->> points (map dir) (apply max) inc))

(defn pt2
  []
  (let [{:keys [folds points]} (input)
        points (reduce fold points folds)]
    (doseq [y (range (max-pos :y points))]
      (println)
      (doseq [x (range (max-pos :x points))]
        (print (if (points {:x x :y y}) "#" "."))))))