(ns advent-of-code-2021.day-2
  (:require [clojure.string :as str]))


(defn parse-line
  [line]
  (let [[d n] (str/split line #" ")]
    [(keyword d) (Integer/parseInt n)]))

(defn input
  []
  (->> (slurp "resources/day_2")
       str/split-lines
       (map parse-line)))


(defn result
  [{:keys [depth pos]}]
  (* depth pos))

(defn pt1
  []
  (-> (fn [coll [dir n]]
        (case dir
          :forward (update coll :pos + n)
          :up (update coll :depth - n)
          :down (update coll :depth + n)))
      (reduce {:depth 0
               :pos   0}
              (input))
      result))

(defn pt2
  []
  (-> (fn [coll [dir n]]
        (case dir
          :forward (-> coll
                       (update :pos + n)
                       (update :depth + (* (:aim coll) n)))
          :up (update coll :aim - n)
          :down (update coll :aim + n)))
      (reduce {:depth 0
               :pos   0
               :aim   0}
              (input))
      result))
