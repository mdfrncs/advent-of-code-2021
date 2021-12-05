(ns advent-of-code-2021.day-5
  (:require [clojure.string :as str]
            [advent-of-code-2021.util :as util]))


(defn line->coords
  [line]
  (let [[_ x y xx yy] (re-matches #"(\d+),(\d+) -> (\d+),(\d+)" line)]
    [[(util/to-int x) (util/to-int y)] [(util/to-int xx) (util/to-int yy)]]))

(defn coords->line
  [[[x1 y1] [x2 y2] :as c]]
  (let [m (if (= x1 x2) (/ (- y1 y2) (- x1 x2)) 0)
        b (- y1 (* m x1))]
    (conj c {:b         b
             :m         m})))

(defn input
  []
  (->> (slurp "resources/day_5")
       str/split-lines
       (map line->coords)
       (map coords->line)))

(defn xmax
  [coords]
  (apply max (mapcat first coords)))

(defn ymax
  [coords]
  (apply max (mapcat second coords)))

(defn bounded?
  [x y [[x1 y1] [x2 y2]]]
  (and (>= y (min y1 y2))
       (<= y (max y1 y2))
       (>= x (min x1 x2))
       (<= x (max x1 x2))))

(defn intersects?
  [x y incl-diag? [[x1 y1] [x2 y2] {:keys [b m]}]]
  (or (= x1 x2 x)
      (= y1 y2 y)
      (and incl-diag?
           (and (= y (+ (* m x) b))))))

(defn pt1
  [& [incl-diag?]]
  (let [coords (input)
        xmax (xmax coords)
        ymax (ymax coords)]

    (let [points (for [y (range (inc ymax))
                       x (range (inc xmax))]
                   (let [included? (every-pred
                                     (partial intersects? x y incl-diag?)
                                     (partial bounded? x y))
                         c (->> coords
                                (filter included?)
                                count)]
                     ;;"pretty" print
                     ((if (= x xmax) println print)
                      (if (= c 0) "." c))
                     [x y c]))]
      (count (filter #(> % 1) (map #(nth % 2) points))))))

(defn pt2
  []
  (pt1 true))
