(ns advent-of-code-2021.day-9
  (:require [clojure.string :as str]
            [advent-of-code-2021.util :as util]
            [clojure.set :as set]
            [clojure.core.async :as a]))

(defn input->map
  [lists]
  (->> (for [x (range (count (first lists)))
             y (range (count lists))]
         [[x y] (nth (nth lists y) x)])
       (into {})))

(defn string->ints
  [line]
  (mapv #(- (int %) 48) line)) ;;char -> int

(defn input
  []
  (->> (slurp "resources/day_9")
       str/split-lines
       (map string->ints)
       input->map))

(defn neighbour-coords
  [[x y]]
  [[x (inc y)] [x (dec y)] [(inc x) y] [(dec x) y]])

(defn low-points-map
  [points]
  (filter (fn [[coord n]]
            (every? (fn [neighbour]
                      (when-let [x (get points neighbour)] (< n x)))
                    (neighbour-coords coord)))
          points))

(defn pt1
  []
  (reduce + (map (comp inc last) (low-points-map (input)))))

(defn neighbours
  [points visited coord]
  (filter (every-pred #(not (get visited %))
                      #(when-let [c (get points %)] (not= c 9)))
          (neighbour-coords coord)))

(defn expand-basin
  [points start]
  (let [expand (a/chan (count points))]
    (a/>!! expand start)
    (loop [result []
           visited #{}]
      (if-let [next (a/poll! expand)]
        (recur (if (not (get visited next))
                 (do
                   (doseq [n (neighbours points visited next)]
                     (a/>!! expand n))
                   (conj result next))
                 result)
               (conj visited next))
        result))))

(defn pt2
  []
  (let [points (input)
        start (low-points-map points)]
    (reduce * (take 3 (reverse (sort (map count (map (comp #(expand-basin points %) first) start))))))))