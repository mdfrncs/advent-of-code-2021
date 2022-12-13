(ns advent-of-code-2022.util
  (:require [clojure.string :as str])
  (:import (java.util PriorityQueue Comparator)))


(defn nth-seq
  [lists n]
  (map #(nth % n) lists))

(defn commas->ints
  [line]
  (map #(Integer/parseInt %) (filter #(not (= "" %)) (str/split line #"([, ])+"))))

(defn spaces->ints
  [line]
  (map #(Integer/parseInt %) (filter #(not (= "" %)) (str/split line #"( )+"))))

(defn to-int
  [i]
  (Integer/valueOf ^String i))

(defn list->map [list key-fn]
  (reduce (fn [acc n]
            (assoc acc (key-fn n) n))
          {}
          list))

;; From day 15, 2021

;;nodes: list of nodes
;;start: actual start node
;; neighbours: fn[nodes, node] -> [adjacent]
;; end?: fn[node] -> the end?
;; cost: fn[from to] -> edge cost; defaults to 1
;; distance: fn[node] -> distance at node
;; setdist!: fn[node dist] -> set distance at node
(defn shortest-path
  [{:keys [nodes start neighbours end? cost distance setdist!]
    :or   {cost (constantly 1)
           end? #(:end? %)
           distance (fn [{:keys [distance]}] @distance)
           setdist! (fn [{:keys [distance]} c] (reset! distance c))}}]
  (doseq [n nodes]
    (setdist! n Long/MAX_VALUE))
  (setdist! start 0)
  (let [expand (PriorityQueue. (reify Comparator
                                 (compare [this x y]
                                   (compare (distance x) (distance y)))))]                                                   ;;need to sort these

    (.add expand start)
    (loop []
      (let [curr (.poll expand)
            nbs (when curr (neighbours nodes curr))] ;;neighbours perfers the map

        (if (or (end? curr) (nil? curr))
          nodes
          (do
            (doseq [n nbs]
              (let [nc (min (distance n) (+ (cost curr n) (distance curr)))]
                (when (< nc (distance n))
                  (setdist! n nc)
                  (.add expand n))))
            (recur)))))))
