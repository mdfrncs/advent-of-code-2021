(ns advent-of-code-2021.day-11
  (:require [clojure.string :as str]))

(defn input
  []
  (->> (slurp "resources/day_11")
       str/split-lines))

(defn example
  []
  (->> (slurp "resources/day_11_example")
       str/split-lines))

(defn char->int
  [c]
  (- (int c) 48))

(defn gridify
  [input]
  (->> (for [x (range 10)
             y (range 10)]
         [[x y] (atom (char->int (nth (nth input y) x)))])
       (into {})))

(defn neighhours
  [[x y]]
  (->> (for [xi '(-1 0 1)
             yi '(-1 0 1)]
         [(+ x xi) (+ y yi)])
       (filter (fn [[x y]]
                 (and (<= 0 x 9)
                      (<= 0 y 9))))))

(defn flash
  [grid coord]
  (let [v (grid coord)]
    (if (<= @v 9)
      0
      (do
        (reset! v 0)
        (reduce (fn [sum c]
                  (swap! (grid c) (fn [e] (if (= 0 e) 0 (inc e)))) ;;increase neighbour energy *unless* already flashed
                  (+ sum (flash grid c)))
                1
                (neighhours coord))))))

(defn step
  [grid]
  (into {} (map (fn [[k v]] [k (swap! v inc)]) grid))
  (reduce (fn [sum coord]
            (+ sum (flash grid coord)))
          0
          (map first grid)))

(defn steps
  [grid n]
  (loop [i 0
         flashes 0]
    (if (= i n)
      flashes
      (recur (inc i)
             (+ flashes (step grid))))))

(defn find-step
  [grid expected-flashes]
  (loop [i 0]
    (if (= expected-flashes (step grid))
      (inc i) ;;steps start at 1
      (recur (inc i)))))

(defn pt1
  []
  (-> (input) gridify (steps 100)))

(defn pt2
  []
  (-> (input) gridify (find-step 100)))


