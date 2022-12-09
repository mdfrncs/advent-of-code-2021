(ns advent-of-code-2022.day-9
  (:require [clojure.string :as str]
            [advent-of-code-2021.util :as util]))

(defn parse-line [line]
  (let [[_ d c] (re-matches #"(.) (\d+)" line)]
    [(keyword d) (util/to-int c)]))

(defn input []
  (->> (slurp "resources/2022/day_9")
       str/split-lines
       (map parse-line)))

(defn flatten [instructions]
  (mapcat (fn [[dir c]] (repeat c [dir 1])) instructions))

(defn touching? [[hx hy :as h] [tx ty :as t]]
  (and (<= (Math/abs (- hy ty)) 1)
       (<= (Math/abs (- hx tx)) 1)))

(defn tds [h t]
  (if (> h t)
    (inc t)
    (dec t)))

(defn follow [[hx hy :as h] [tx ty :as t]]
  (cond
    (touching? h t) t
    (= hx tx) [tx (tds hy ty)]
    (= ty hy) [(tds hx tx) ty]
    :default [(tds hx tx) (tds hy ty)]))

(defn move-head [[x y] dir]
  (case dir
    :R [(inc x) y]
    :L [(dec x) y]
    :U [x (inc y)]
    :D [x (dec y)]))

(defn pt1 []
  (->> (input)
       flatten
       (reduce (fn [{:keys [head tail visited]} [dir _]]
                 (let [nh (move-head head dir)
                       nt (follow nh tail)]
                   {:head    nh
                    :tail    nt
                    :visited (conj visited nt)
                    }))
               {:head    [0 0]
                :tail    [0 0]
                :visited #{[0 0]}})
       :visited
       count))


;; PT2


(defn move-rope [head rope]
  (rest (reduce (fn [new-rope tail]
                  (let [nt (follow (last new-rope) tail)]
                    (conj new-rope nt)))
                [head]
                rope)))


(defn pt2 []
  (->> (input)
       flatten
       (reduce (fn [{:keys [head rope visited]} [dir _]]
                 (let [nh (move-head head dir)
                       nr (move-rope nh rope)]
                   {:head    nh
                    :rope    nr
                    :visited (conj visited (last nr))
                    }))
               {:head    [0 0]
                :rope    (repeat 9 [0 0])
                :visited #{[0 0]}})
       :visited
       count))


