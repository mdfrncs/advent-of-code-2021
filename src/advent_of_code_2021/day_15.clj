(ns advent-of-code-2021.day-15
  (:require [clojure.string :as str])
  (:import (java.util PriorityQueue Comparator)))

(defn char->int
  [c]
  (- (int c) 48))

(defn node
  [lines x y]
  (let [risk (char->int (nth (nth lines y) x))]
    {:risk   risk
     :x x
     :y y
     :start? (= 0 x y)
     :end?   (= (dec (count lines)) x y)
     :cost   (atom (if (= 0 x y) 0 Long/MAX_VALUE))}))

(defn in
  [file]
  (let [lines (->> (slurp file)
                   str/split-lines)]
    lines))

(defn nodify
  [lines]
  (->> (for [y (range (count lines))
             x (range (count (first lines)))]
         [[x y] (node lines x y)])
       (into {})))

(defn input [] (in "resources/day_15"))
(defn example [] (in "resources/day_15_example"))

(defn neighbours
  [nodes {:keys [x y]}]
  (->> [      [0 1]
        [-1 0]      [1 0]
              [0 -1]    ]
       (map (fn [[xi yi]] [(+ x xi) (+ y yi)]))
       (keep nodes)))

(defn shortest-path
  [nodes]

  ;;nodes to explore
  (let [expand (PriorityQueue. (reify Comparator
                                 (compare [this x y]
                                   (compare @(:cost x) @(:cost y)))))]                                                   ;;need to sort these

    (.add expand (get nodes [0 0]))
    (loop []
      (let [curr (.poll expand)
            nbs (neighbours nodes curr)]

        (if (:end? curr)
          nodes
          (do
            (doseq [n nbs]
              (let [nc (min @(:cost n) (+ (:risk n) @(:cost curr)))]
                (when (< nc @(:cost n))
                  (reset! (:cost n) nc)
                  (.add expand n))))
            (recur)))))))

(defn solve
  [nodes]
  @(->> nodes shortest-path (map second) (filter :end?) first :cost))

(defn pt1
  []
  (-> (input) nodify solve))

(defn expand-node
  [size {:keys [risk x y] :as node}]
  (for [xi (range 5)
        yi (range 5)]
    (-> node
        (assoc :risk (+ 1 (mod (- (+ risk xi yi) 1) 9)))
        (assoc :cost (atom (if (= 0 x y xi yi) 0 Long/MAX_VALUE)))
        (assoc :x (+ x (* xi size)))
        (assoc :y (+ y (* yi size)))
        (assoc :end? false))))

(defn expand-map
  [size nodes]
  (->> (mapcat (partial expand-node size) (map second nodes))
       (reduce (fn [acc {:keys [x y] :as node}]
                 (assoc acc [x y] node))
               {})))

(defn set-end
  [size nodes]
  (update nodes [(dec size) (dec size)] assoc :end? true))

(defn pt2
  []
  (let [input (input)
        nodes  (expand-map (count input) (nodify input))]
    (->> nodes
         (set-end (* (count input) 5))
         solve)))

