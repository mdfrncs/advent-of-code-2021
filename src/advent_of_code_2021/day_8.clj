(ns advent-of-code-2021.day-8
  (:require [clojure.string :as str]
            [advent-of-code-2021.util :as util]
            [clojure.set :as set]))

(defn spaces->seq
  [line]
  (-> (str/trim line) (str/split #"\s")))

(defn parse-line
  [line]
  (let [[input output] (str/split line #"\|")]
    [(spaces->seq input) (spaces->seq output)]))

(defn input
  []
  (->> (slurp "resources/day_8")
       str/split-lines
       (map parse-line)))

(defn find
  [seq length]
  (map #(into #{} %) (dedupe (sort (filter #(= length (count %)) seq)))))

(defn diff
  [& sets]
  (set/difference (apply set/union sets) (apply set/intersection sets)))

(defn decoder
  [in]
  (let [one (first (find in 2))
        four (first (find in 4))
        seven (first (find in 3))
        eight (first (find in 7))
        d5 (find in 5)
        d6 (find in 6)

        a (set/difference seven one)
        b-e (set/difference (apply diff d5) one)
        b (set/intersection b-e four)
        e (set/difference b-e b)
        d (set/difference four (set/union one b))
        g (set/difference eight (set/union one a b e d))
        c (set/difference (apply diff d6) d e)
        f (set/difference one c)]

    (->> {\a a
          \b b
          \e e
          \d d
          \g g
          \c c
          \f f}
         (map (fn [[k v]] [(first v) k]))
         (into {}))))


(def digits
  (->> {"abcefg"  0
        "cf"      1
        "acdeg"   2
        "acdfg"   3
        "bcdf"    4
        "abdfg"   5
        "abdefg"  6
        "acf"     7
        "abcdefg" 8
        "abcdfg"  9}
       (map (fn [[k v]] [(into #{} k) v]))
       (into {})))

(defn out->digit
  [decoder out]
  (get digits (into #{} (map #(get decoder %) out))))

(defn decode
  [[in out]]
  (->> out
       (map (partial out->digit (decoder in)))
       (reduce str)
       Integer/parseInt))

(defn pt1
  []
  (count (filter #(#{2 4 3 7} (count %)) (mapcat second (input)))))

(defn pt2
  []
  (reduce + (map decode (input))))