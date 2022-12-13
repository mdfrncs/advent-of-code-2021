(ns advent-of-code-2022.day-11
  (:require [clojure.string :as str]
            [advent-of-code-2021.util :as util]))


(defn operation->code [[a b c]]
  [b a c])

(defn parse-monkey [idx line]
  (let [regexes [#"Starting items: (.*)"
                 #"Operation: new = (.*)"
                 #"Test: divisible by (\d*)"
                 #"If true: throw to monkey (\d*)"
                 #"If false: throw to monkey (\d*)"]
        [items op test tr fl] (->> (str/split-lines line)
                                   (map str/trim)
                                   (map (fn [re line] (re-matches re line)) regexes)
                                   (map last))]
    {:raw       line
     :index     idx
     :items     (->> items util/commas->ints (into []) atom)
     :operation (-> op (str/split #" ") operation->code)
     :test      (-> test util/to-int)
     :on-true   (-> tr util/to-int)
     :on-false  (-> fl util/to-int)
     :count     (atom 0)}))

(defn input []
  (let [s (slurp "resources/2022/day_11")
        spl (str/split s #"Monkey \d:")]
    (->> spl
         rest
         (map str/trim)
         (map-indexed parse-monkey))))

(defn op [[fn a b]  old]
  ((resolve (symbol fn))
   (if (= a "old") old (util/to-int a))
   (if (= b "old") old (util/to-int b))))

(defn pass [item to monkeys]
  (let [monkey (nth monkeys to)]
    (swap! (:items monkey) conj item)))

(defn run-monkey [{:keys [items operation test on-true on-false]} item monkeys bored-fn]
  (let [up (op operation item)
        bored (bored-fn up)]
    (if (= 0 (mod bored test))
      (pass bored on-true monkeys)
      (pass bored on-false monkeys))))

(defn round [monkeys bored-fn]
  (doseq [{:keys [items count] :as monkey} monkeys]
    (let [oitems @items]
      (reset! items [])
    (doseq [item oitems]
      (swap! count inc)
      (run-monkey monkey item monkeys bored-fn)))))

(defn pt1 []
  (let [monkeys (input)]
    (doseq [_ (range 20)]
      (round monkeys #(int (/ % 3))))
    (->> monkeys
         (map (comp deref :count))
         sort
         reverse
         (take 2)
         (reduce *))))


(defn lcm [monkeys] ;;lcm-ish
  (->> monkeys
       (map :test)
       (reduce *)))

(defn pt2 []
  (let [monkeys (input)
        lcm (lcm monkeys)]
    (doseq [_ (range 10000)]
      (round monkeys #(mod % lcm)))
    (->> monkeys
         (map (comp deref :count))
         sort
         reverse
         (take 2)
         (reduce *))))