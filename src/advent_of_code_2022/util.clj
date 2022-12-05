(ns advent-of-code-2022.util
  (:require [clojure.string :as str]))


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