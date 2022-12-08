(ns advent-of-code-2022.day-8
  (:require [clojure.string :as str]
            [advent-of-code-2021.util :as util]))

(defn parse-line [line]
  (->> line
       (map (comp util/to-int str identity))))

(defn input []
  (->> (slurp "resources/2022/day_8")
       str/split-lines
       (map parse-line)))

(defn height [input [x y]]
  (-> input (nth y) (nth x)))

(defn slices [input [x y]]
  (let [height (partial height input)
        mx (count (first input))
        my (count input)]
  [(->> (range y) (map #(height [x %])) reverse)
   (->> (range (inc y) my) (map #(height [x %])))
   (->> (range x) (map #(height [% y])) reverse)
   (->> (range (inc x) mx) (map #(height [% y])))]))

(defn points [input]
  (let [mx (count (first input))
        my (count input)]
    (for [x (range mx)
          y (range my)]
      [x y])))

(defn visible? [input [x y :as p]]
  (->> (slices input p)
       (map #(if (seq %) (reduce max %) -1))
       (apply min)
       (> (height input p))))

(defn pt1 []
  (let [input (input)]
    (->> (points input)
         (filter (partial visible? input))
         count)))

;; PT2

(defn visible-trees [v trees]
  (reduce (fn [c tree]
            (if (> v tree)
              (inc c)
              (reduced (inc c))))
          0
          trees))

(defn scenic-score [input [x y :as p]]
  (->> (slices input p)
       (map (partial visible-trees (height input p)))
       (reduce *)))

(defn pt2 []
  (let [input (input)]
    (->> (points input)
         (map (partial scenic-score input))
         (reduce max))))
