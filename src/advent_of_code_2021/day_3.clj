(ns advent-of-code-2021.day-3
  (:require [clojure.string :as str]))

(defn parse-line
  [line]
  (mapv #(- (int %) 48) line))

(defn input
  []
  (->> (slurp "resources/day_3")
       str/split-lines
       (map parse-line)))

(defn nth-seq
  [lists n]
  (map #(nth % n) lists))

(defn binary->dec
  [bits]
  (Integer/parseInt (str/join bits) 2))

(defn width
  [input]
  (count (first input)))

(defn lcd
  [{one 1 zero 0}]
  (if (<= zero one)
    0 1))

(defn gcd
  [{one 1 zero 0}]
  (if (>= one zero)
    1 0))

(def epislon gcd)
(def gamma lcd)
(def oxygen lcd)
(def co2 gcd)

(defn freqs->rating
  [freqs pred]
  (->> freqs (map pred) binary->dec))

(defn pt1
  []
  (let [input (input)
        freqs (map (comp frequencies (partial nth-seq input))
                   (range (width input)))]
    (* (freqs->rating freqs epislon)
       (freqs->rating freqs gamma))))


(defn find
  [nums pred]
  (reduce (fn [nums pos]
            (let [digit (-> nums (nth-seq pos) frequencies pred)
                  filtered (filter #(= digit (nth % pos)) nums)]
              (if (seq (rest filtered))
                filtered
                (reduced (first filtered)))))
          nums
          (range (width nums))))

(defn pt2
  []
  (let [input (input)]
    (let [oxy (find input oxygen)
          co2 (find input co2)]
    (* (binary->dec oxy) (binary->dec co2)))))