(ns advent-of-code-2021.day-1
  (:require [clojure.string :as str]))


(defn input []
  (->> (slurp "resources/day_1")
       str/split-lines
       (mapv #(Integer/parseInt %))))

(defn count-increased
  [input]
  (reduce +
          (map #(if (< %1 %2) 1 0)
               input
               (rest input))))

(defn pt1
  []
  (count-increased (input)))

(defn pt2
  []
  (let [input (input)]
    (-> (map +
             input
             (rest input)
             (rest (rest input)))
        count-increased)))
