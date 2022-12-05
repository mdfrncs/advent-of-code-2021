(ns advent-of-code-2022.day-1
  (:require [clojure.string :as str]
            [advent-of-code-2021.util :as util]))

(defn input []
  (->> (slurp "resources/2022/day_1")
       str/split-lines
       (partition-by empty?)
       (filter #(not (empty? (first %))))
       (map #(map util/to-int %))))

(defn pt1 []
  (->> (input)
       (map #(reduce + %))
       (apply max)))

(defn pt2 []
  (->> (input)
       (map #(reduce + %))
       sort
       reverse
       (take 3)
       (reduce +)))