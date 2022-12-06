(ns advent-of-code-2022.day-6
  (:require [clojure.string :as str]
            [advent-of-code-2021.util :as util]))

(defn input []
  (->> (slurp "resources/2022/day_6")
       (map identity)))

(defn solve [size]
  (let [input (input)]
  (->> (range size)
       ;; generate n, n-1 to n-size char lists from input
       (map #(drop % input))
       ;; combine 'size' chars into sets
       (apply map (fn [& args] (into #{} args)))
       ;; order matters
       (map-indexed vector) ;; [idx, set(char)]
       ;; first set with 'size' characters is the first marker
       (filter (fn [[_ s]] (= size (count s))))
       first
       ;; unwrap index
       first
       ;; first set is actually at index size
       (+ size))))

(defn pt1 []
  (solve 4))

(defn pt2 []
  (solve 14))
