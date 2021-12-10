(ns advent-of-code-2021.day-10
  (:require [clojure.string :as str]
            [advent-of-code-2021.util :as util]
            [clojure.set :as set]
            [clojure.core.async :as a]))

(defn input
  []
  (->> (slurp "resources/day_10")
       str/split-lines))

(defn example
  []
  (->> (slurp "resources/day_10_example")
       str/split-lines))

(def pairs
  {\[ \]
   \( \)
   \< \>
   \{ \}})

(defn errors
  [line]
  (reduce (fn [stack sym]
            (cond
              (pairs sym) (cons (pairs sym) stack) ;;opening bracket
              (= (first stack) sym) (rest stack)   ;;closing matches expected
              (first stack) (reduced sym)          ;;incorrect closing bracket
              :default (reduced :error)))          ;;closing with no opening
         '()
          line))

(def cost
  {\) 3
   \] 57
   \} 1197
   \> 25137})

(defn pt1
  []
  (->> (input)
       (map errors)
       (filter char?)
       (map cost)
       (reduce +)))

(def char-cost-pt2
  {\) 1
   \] 2
   \} 3
   \> 4})

(defn incomplete-cost
  [line]
  (reduce (fn [sum cost]
            (+ (* 5 sum) cost))
          0
          (map char-cost-pt2 line)))

(defn middle
  [list]
  (nth list (int (/ (count list) 2))))

(defn pt2
  []
  (->> (input)
       (map errors)
       (filter seq?)
       (map incomplete-cost)
       sort
       middle))
