(ns advent-of-code-2022.day-13
  (:require [clojure.string :as str]
            [advent-of-code-2022.util :as util]
            [clojure.edn :as edn])
  (:import (clojure.lang PersistentVector)))

(defn input []
  (->> (slurp "resources/2022/day_13")
       str/split-lines
       (filter seq)
       (map edn/read-string)))

(defmulti right-order (fn [a b] [(type a) (type b)]))

(defmethod right-order [Long PersistentVector] [a b] (right-order [a] b))
(defmethod right-order [PersistentVector Long] [a b] (right-order a [b]))
(defmethod right-order [Long Long] [a b]
  (cond
    (< a b) :right
    (> a b) :wrong
    :else :cont))

(defmethod right-order [PersistentVector PersistentVector] [a b]
  (reduce (fn [_ i]
            (let [ugh (cond
                        (>= i (count a)) :right
                        (>= i (count b)) :wrong
                        :else (right-order (nth a i) (nth b i))
                        )]
              (if (= ugh :cont) ugh (reduced ugh))))
          :cont
          (range (max (count a) (count b)))))

(defn pt1 []
  (->> (input)
       (partition 2)
       (map (fn [[a b]] (right-order a b)))
       (map {:right true :wrong false})
       (map-indexed vector)
       (filter second)
       (map (comp inc first))
       (reduce +)))


(defn right-ordered [a b]
  (case (right-order a b)
    :right -1
    :wrong 1
    :cont 0))

(defn pt2 []
  (->> (input)
       (concat [[[2]] [[6]]])
       (sort right-ordered)
       (map-indexed vector)
       (filter (comp #{[[2]] [[6]]} second))
       (map (comp inc first))
       (reduce *)))



