(ns advent-of-code-2021.day-4
  (:require [clojure.string :as str]
            [advent-of-code-2021.util :as util]))


(defn width
  [input]
  (count (first input)))

(defn line->state
  [line]
  (mapv
    (fn [i] {:value i :called (atom false)})
    (util/spaces->ints line)))

(defn lines->board
  [lines]
  (mapv
    line->state
    (rest lines)))

(defn lines->game-input
  "First line is input sequence; boards are 5x5 so there's that"
  [lines]
  (let [inseq (util/commas->ints (first lines))
        boards (mapv lines->board (partition 6 (rest lines)))]
    [inseq boards]))

(defn input
  []
  (->> (slurp "resources/day_4")
       str/split-lines
       lines->game-input))

;; Where do I turn in my clojure credentials
(defn update-boards
  [c boards]
  (doseq [board boards]
    (doseq [row board]
      (doseq [{:keys [value called]} row]
        (if (= value c)
          (reset! called true))))))

(defn winner?
  [board]
  (let [bingo? (fn [seq] (every? (fn [{called :called}] @called) seq))
        columns (map #(util/nth-seq board %) (range (width board)))]
    (when (some bingo? (concat board columns))
      board)))

(defn sum-unmarked
  [board]
  (->> board
       (mapcat identity)
       (filter #(not @(:called %)))
       (map :value)
       (reduce + 0)))

(defn play-game
  [[inseq boards] & [last-wins?]]
  (reduce
    (fn [boards called]
      (update-boards called boards)
      (if-let [winner (and (or (not last-wins?)
                               (= 1 (count boards)))
                           (some winner? boards))]
        (reduced (* called (sum-unmarked winner)))
        (filter (comp not winner?) boards)))
    boards
    inseq))

(defn pt1
  []
  (play-game (input)))

(defn pt2
  []
  (play-game (input) true))