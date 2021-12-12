(ns advent-of-code-2021.day-12
  (:require [clojure.string :as str]
            [advent-of-code-2021.util :as util]
            [clojure.set :as set]
            [clojure.core.async :as a]))

(defn parse-line
  [line]
  (rest (re-matches #"([a-zA-Z+]+)-([a-zA-Z+]+)" line)))


(defn input
  []
  (->> (slurp "resources/day_12")
       str/split-lines
       (map parse-line)))

(defn example
  []
  (->> (slurp "resources/day_12_example")
       str/split-lines
       (map parse-line)))

(defn edges->adj-list
  [input]
  (reduce (fn [acc [s e]]
            (-> acc
                (update s conj e)
                (update e conj s)))
          {}
          input))

(def ^:dynamic pt2? false)

(defn lowercase?
  [str]
  (= (str/lower-case str) str))

(defn expand?
  [path n]
  (cond
    (= n "start") false
    (not (lowercase? n)) true                ;;Large room
    (not (some #(= % n) path)) true          ;;Not yet visited on this path
    pt2? (->> (filter lowercase? path)       ;;One small room duplicate allowed
              frequencies
              (some #(> (second %) 1))
              not)
    :else false))


(defn pathfinder
  ([graph] (pathfinder graph ["start"] "start"))
  ([graph path curr]
   (if (= curr "end")
     [path]
     (reduce (fn [paths n]
               (if (expand? path n)
                 (concat paths (pathfinder graph (conj path n) n))
                 paths))
             []
             (graph curr)))))

(defn solve
  [input]
  (-> input edges->adj-list pathfinder sort dedupe count))

(defn pt1
  []
  (solve (input)))

(defn pt2
  []
  (binding [pt2? true]
    (solve (input))))

