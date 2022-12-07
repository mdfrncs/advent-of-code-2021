(ns advent-of-code-2022.day-7
  (:require [clojure.string :as str]
            [advent-of-code-2021.util :as util]))

(defn input []
  (->> (slurp "resources/2022/day_7")
       str/split-lines))

(defn parse
  [line]
  (let [[a b c] (str/split line #"( )+")]
    (case a
      "$" [:command b c]
      "dir" [:directory b]
      [:file b a])))

(defn new-node [parent name size]
  (atom {:parent parent
         :label name
         :size (if size (util/to-int size))
         :dir? (not size)
         :children {}}))

(defn add-child! [node [instr a b]]
  (swap! node (fn [{:keys [children] :as n}]
                (if (children a)
                  n
                  (update n :children assoc a (new-node node a b)))))
  node)

(defn dir? [node]
  (:dir? @node))

(defn children [node]
  (vals (:children @node)))

(defn update-sizes [node]
  (if (dir? node)
    (let [s (->> node children (map update-sizes) (reduce +))]
      (swap! node assoc :size s)
      s)
    (:size @node)))

(defn build-tree [lines]
  (let [instructions (map parse lines)
        root (new-node nil "/" nil)]
    (reduce (fn [node [inst a b :as c]]
              (case inst
                :command (case a
                           "cd" (case b
                                  "/" root
                                  ".." (:parent @node)
                                  (get (:children @node) b))
                           "ls" node)
                :directory (add-child! node c)
                :file (add-child! node c)))
            root
            instructions)
    (update-sizes root)
    root))

(defn directories [node]
  (conj (mapcat directories (filter dir? (children node))) node))


(defn pt1 []
  (let [root (build-tree (input))]
    (->> (directories root)
         (map (comp :size deref))
         (filter #(<= % 100000))
         (reduce +))))

(defn pt2 []
  (let [root (build-tree (input))
        req (- 30000000 (- 70000000 (:size @root)))]
    (->> (directories root)
         (map (comp :size deref))
         (filter #(> % req))
         sort
         first)))