(ns advent-of-code-2021.day-14
  (:require [clojure.string :as str]))

(defn line->rule
  [line]
  (let [[_ k v] (re-matches #"([A-Z]+) -> ([A-Z])" line)]
    [(map identity k) (first (map identity v))]))

(defn in
  [file]
  (let [lines (->> (slurp file)
                   str/split-lines)]
    {:template (first lines)
     :rules    (into {} (map line->rule (rest (rest lines))))}))

(defn input [] (in "resources/day_14"))
(defn example [] (in "resources/day_14_example"))

(def pair->frqs
  "Recursively turn pair [a b] with n steps into letter frequencies using insertion rules"
  (memoize
    (fn [rules n a b]
      (if (= 0 n)
        (frequencies [a b])
        (let [i (rules [a b])]
          (merge-with +
                      (pair->frqs rules (dec n) a i)
                      (pair->frqs rules (dec n) i b)
                      {i -1}))))))


(defn solve
  [input n]
  (let [{:keys [template rules]} input
        ;;Map all pairs through pair->freqs and add it all together
        freqs-w-dups (apply merge-with + (map (partial pair->frqs rules n) template (rest template)))
        ;;Remove double counted letters
        freqs (->> template rest reverse rest frequencies (merge-with - freqs-w-dups) (map second))]
    (- (apply max freqs) (apply min freqs))))

(defn pt1
  []
  (solve (input) 10))

(defn pt2
  []
  (solve (input) 40))


