(ns advent-of-code-2021.day-6
  (:require [clojure.string :as str]
            [advent-of-code-2021.util :as util]))

(defn input
  []
  (->> (slurp "resources/day_6")
       util/commas->ints))

(def populate
  (memoize
    (fn [days fish]
      (cond
        (= 0 days) 1
        (= 0 fish) (+ (populate (dec days) 6) (populate (dec days) 8))
        :else (populate (dec days) (dec fish))))))

(defn pt1
  []
  (reduce + (map (partial populate 80) (input))))

(defn pt2
  []
  (reduce + (map (partial populate 256) (input))))