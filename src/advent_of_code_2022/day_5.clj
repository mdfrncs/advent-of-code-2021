(ns advent-of-code-2022.day-5
  (:require [clojure.string :as str]
            [advent-of-code-2021.util :as util]))

(def ^:dynamic mode :example)
(defn input []
  (slurp (if (= mode :example ) "resources/2022/day_5_example" "resources/2022/day_5")))

(defn crate-at
  [lines crane pos]
  (let [crane-pos (inc (* crane 4))
        line (nth lines pos)]
    (when (< crane-pos (count line))
      (let [char (nth line crane-pos)]
        (if (= char \space) nil char)))))

;; returns: list of crates at crane
(defn parse-crane [lines depth crane]
  (->> (range depth)
       (map (partial crate-at lines crane))
       (filter (comp not nil?))))

;; [depth, # cranes]
(defn crane-info [lines]
  (reduce (fn [depth line]
            (if (= \1 (second line))
              (reduced [depth (-> line str/trim last str util/to-int)])
              (inc depth)))
          0 lines))

(defn input->cranes [lines depth num]
  (->> (range num)
       (map (partial parse-crane lines depth))
       (map atom)))

;; list of [move from to] instructions
(defn parse-instruction [line]
  (->> line
       (re-matches #"move (\d*) from (\d*) to (\d*)")
       rest
       (map util/to-int)))

(defn input->instructions [lines depth]
  (map parse-instruction (drop (+ depth 2) lines)))

(defn move [cranes m from to]
  (doseq [i (range m)]
    (let [val (first @(nth cranes from))]
      (swap! (nth cranes from) rest)
      (swap! (nth cranes to) conj val))))

(defn move-9001
  [cranes m from to]
  (let [val (take m @(nth cranes from))]
    (swap! (nth cranes from) (partial drop m))
    (swap! (nth cranes to) (partial concat val))))

(defn read-puzzle []
  (let [lines (-> (input) str/split-lines)
        [depth c] (crane-info lines)]
    {:cranes (input->cranes lines depth c)
     :instructions (input->instructions lines depth)}))

(defn cranes->answer [cranes]
  (apply str (map first (map deref cranes))))

(defn pt1 []
  (let [{:keys [cranes instructions]} (read-puzzle)]
    (doseq [[m f t] instructions]
      (move cranes m (dec f) (dec t)))
    (cranes->answer cranes)))

(defn pt2 []
  (let [{:keys [cranes instructions]} (read-puzzle)]
    (doseq [[m f t] instructions]
      (move-9001 cranes m (dec f) (dec t)))
    (cranes->answer cranes)))


