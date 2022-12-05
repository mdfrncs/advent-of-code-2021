(ns advent-of-code-2022.day-2
  (:require [clojure.string :as str]
            [advent-of-code-2021.util :as util]))


(def opp-key
  {
   \A :rock
   \B :paper
   \C :scissors
   })

(def strat-play
  {
   \X :rock
   \Y :paper
   \Z :scissors
   })

(def rules
  {:rock :scissors
  :scissors :paper
  :paper :rock})

(def rules-inv
  (into {} (map (fn [c] [(val c) (key c)]) rules)))

(def outcomes
  {:win 6
   :tie 3
   :lose 0})

(def plays
  {:rock 1
  :paper 2
  :scissors 3})

(defn outcome
  [opp you]
  (cond
    (= (rules you) opp) :win
    (= opp you) :tie
    :else :lose))

(defn hand->outcome
  [[opp you]]
  (outcome opp you)
  )

(defn line->details
  [line]
  (let [opponent (-> (first line) opp-key)
        you (-> (last line) strat-play)]
   [opponent
    you
    (outcome opponent you)]))

(defn input []
  (->> (slurp "resources/2022/day_2")
       str/split-lines))

(defn details->points
  [[_ you outcome]]
  (+ (plays you) (outcomes outcome)))

(defn pt1
  []
  (->> (input)
       (map line->details)
      (map details->points)
       (reduce +)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def decode-strat
  {\X :lose
   \Y :tie
   \Z :win})

(defn outcome->play
  [opp strat]
  (case strat
    :win (rules-inv opp)
    :lose (rules opp)
    :tie opp))


(defn line->details2
  [line]
  (let [opponent (-> (first line) opp-key)
        outcome (-> (last line) decode-strat)]
    [opponent
     (outcome->play opponent outcome)
     outcome]))

(defn pt2
  []
  (->> (input)
       (map line->details2)
       (map details->points)
       (reduce +)))