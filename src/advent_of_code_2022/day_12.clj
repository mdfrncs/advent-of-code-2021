(ns advent-of-code-2022.day-12
  (:require [clojure.string :as str]
            [advent-of-code-2022.util :as util]))

(defn input []
  (->> (slurp "resources/2022/day_12")
       str/split-lines
       (map #(map identity %))))


(defn get [lines [x y]]
  (nth (nth lines y) x))

(defn normalize [a]
  (cond
    (= a \S) \a
    (= a \E) \z
    :default a))

(defn points [lines]
  (let [ym (count lines)
        xm (count (first lines))]
    (for [x (range xm)
          y (range ym)]
      {:p [x y]
       :v (->> [x y] (get lines) normalize int)
       :r (->> [x y] (get lines))
       :distance (atom 0)}))) ;;for shortest-path algorithm

(defn adjacent? [a b]
  (let [a (:v a)
        b (:v b)]
  (or (= a b)
      (= (inc a) b)
      (> a b))))

(defn neighbours [nodes adjacent?]
  (let [node-map (util/list->map nodes :p)]
    (fn [_ {:keys [p] :as node}]
      (let [[x y] p]
        (->> [[0 1] [-1 0] [1 0] [0 -1]]
             (map (fn [[xi yi]] [(+ x xi) (+ y yi)]))
             (keep node-map)
             (filter (partial adjacent? node)))))))

(defn solve [start? end? adjacent?]
  (let [nodes (-> (input) points)
        start (first (filter start? nodes))]
    (util/shortest-path {:nodes      nodes
                         :start      start
                         :end?       end?
                         :neighbours (neighbours nodes adjacent?)})
    (->> nodes
         (filter end?)
         (map (comp deref :distance))
         sort
         first)))


(defn pt1 []
  (solve #(= \S (:r %))
         #(= \E (:r %))
         adjacent?))

;; when traversing from \E to \a adjacency rules are inverse
(defn adjacent-reverse? [a b]
  (let [a (:v a)
        b (:v b)]
    (or (= a b)
        (= (dec a) b)
        (< a b))))

(defn pt2 []
  (solve #(= \E (:r %))
         #(= (int \a) (:v %))
         adjacent-reverse?))