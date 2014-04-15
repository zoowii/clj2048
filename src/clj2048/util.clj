(ns clj2048.util
  (:gen-class)
  (:require [clojure.core.matrix :as matrix]))

(defn swap-seq [col i j]
  ; 交换集合col的第i和j位
  (assoc col i (nth col j) j (nth col i)))

(defn fill-zeros [col n]
  ;; 在col后面填上0，直到col长度达到n
  {:pre (>= n (count col))}
  (concat col (repeat (- n (count col)) 0)))

(defn seq-index [col]
  ;; (seq-index [ a b c ]) => [ [ 0 a] [1 b] [3 c]]
  (map #(vector %1 %2) (range) col))

(defn get-positions-of-value [col val]
  (map first
       (filter #(= (second %) val)
               (seq-index col))))

(defn iter-matrix [m]
  ;; (iter-matrix [[1 2 3] [4 5 6] [7 8 9]]) => [[0 1] [1 2] ... ]
  (for [i (range (count m))
        j (range (count (matrix/get-column m 0)))]
    [(+ j (* (count m) i)) (matrix/mget m i j)]))

(defn filter-matrix [fn m]
  (let [vals (map second (iter-matrix m))]
    (filter fn vals)))

(defn zero-int-matrix
  ([rows] (zero-int-matrix rows rows))
  ([rows cols]
   (matrix/matrix (repeat rows (repeat cols 0)))))