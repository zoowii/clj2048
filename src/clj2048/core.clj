(ns clj2048.core
  (:gen-class)
  (:import (javax.swing JPanel SwingUtilities JLabel JFrame)
           (java.awt.event KeyEvent))
  (:require [clojure.core.matrix :as matrix]
            [clojure.core.matrix.operators :as m-op])
  (:use gui.core))

(def state (atom {:score 0
                  :grid  (matrix/matrix
                           [[0 2 0 0]
                            [0 0 0 2]
                            [0 0 0 0]
                            [0 0 0 0]])}))

(defmulti get-rows-of-direction :direction) ;; 把grid转成各个方向的rows

(defmethod get-rows-of-direction :up [{grid :grid}]
  (map #(matrix/get-column grid %) (range 4)))

(defmethod get-rows-of-direction :down [{grid :grid}]
  (map reverse
       (map #(matrix/get-column grid %) (range 4))))

(defmethod get-rows-of-direction :left [{grid :grid}]
  (map #(matrix/get-row grid %) (range 4)))

(defmethod get-rows-of-direction :right [{grid :grid}]
  (map reverse
       (map #(matrix/get-row grid %) (range 4))))

(defmulti parse-direction-rows-to-grid :direction) ;; 把各个方向的rows转成grid

(defmethod parse-direction-rows-to-grid :up [{rows :rows}]
  (map #(matrix/get-column rows %) (range 4)))

(defmethod parse-direction-rows-to-grid :down [{rows :rows}]
  (map #(matrix/get-column (map reverse rows) %) (range 4)))

(defmethod parse-direction-rows-to-grid :left [{rows :rows}]
  (map #(matrix/get-row rows %) (range 4)))

(defmethod parse-direction-rows-to-grid :right [{rows :rows}]
  (map #(matrix/get-row (map reverse rows) %) (range 4)))

(defn- get-rows-of-grid-direction [grid direction]
  ;; 按照方向把grid切分成上下或左右的序列，注意，正反方向是不一样的
  (matrix/matrix
    (get-rows-of-direction {:grid grid :direction direction})))

(defn- swap-seq [col i j]
  ; 交换集合col的第i和j位
  (assoc col i (nth col j) j (nth col i)))

(defn- fill-zeros [col n]
  ;; 在col后面填上0，直到col长度达到n
  {:pre (>= n (count col))}
  (concat col (repeat (- n (count col)) 0)))

(defn- eat-zeros [row]
  ;; “吃”掉row中的0
  (fill-zeros (filter #(> % 0) row) (count row)))

(defn- eat-row
  ;; 对于row，从低索引位置开始“吃”大索引位置
  ([row] (eat-row row 0 0))
  ([row idx score]
   (let [row (vec (eat-zeros row))]
     (if (>= idx (dec (count (filter #(> % 0) row))))
       {:row row :score score}
       (let [cur (nth row idx)
             next (nth row (inc idx))]
         (if (= cur next)
           (recur (assoc row idx (* 2 cur) (inc idx) 0) 0 (+ score cur))
           (recur row (inc idx) score)))))))

(defn move-grid [state direction]
  ;; 按某个方向变换格子，但是还没有增加新的格子
  (let [rows (get-rows-of-grid-direction
               (:grid state)
               direction)
        eat-result (map eat-row rows)
        score (+ (:score state) (apply + (map :score eat-result)))
        rows (matrix/matrix (map :row eat-result))
        grid (-> {:rows rows :direction direction}
                 parse-direction-rows-to-grid
                 matrix/matrix)]
    (assoc state :grid grid :score score)))

(defn- seq-index [col]
  ;; (seq-index [ a b c ]) => [ [ 0 a] [1 b] [3 c]]
  (map #(vector %1 %2) (range) col))

(defn- get-positions-of-value [col val]
  (map first
       (filter #(= (second %) val)
               (seq-index col))))

(defn add-2-to-grid [state]
  ;; 在格子中剩下的空白位置随机插入一个新的2
  (let [col (flatten (:grid state))
        zero-positions (get-positions-of-value col 0)
        rand-pos (rand-nth zero-positions)
        [i j] [(int (/ rand-pos 4)) (mod rand-pos 4)]
        grid (matrix/mset (:grid state) i j 2)
        state (assoc state :grid grid)]
    state))

(defn ended-grid? [state]
  "判断是否无路可走，也就是游戏是否结束了")

(defn iter-matrix [m]
  ;; iterate matrix
  (for [i (range (count m))
        j (range (count (matrix/get-column m 0)))]
    [(+ j (* (count m) i)) (matrix/mget m i j)]))

(defn display-grid [panel state]
  ;; 在panel中显示grid
  (let [coms (.getComponents panel)
        grid (:grid state)]
    (doseq [[idx val] (iter-matrix grid)]
      (.setText (nth coms idx) (str val)))
    ;; 这几行代码会导致非常奇怪的问题，第一次执行时for循环会执行，以后不会，
    ;; 可能是因为Clojure把它当作无副作用的函数调用，然后就缓存了 ???
    ;(for [[idx val] (iter-matrix grid)]
    ;  (let [com (nth coms idx)]
    ;    (.setText com (str val))))
    ))

(defn- display-score [pane state]
  ;; 把分数更新显示到界面上
  (let [container (.getParent pane)
        score-label (.getComponent container 0)]
    (.setText score-label (str "Score: " (:score state)))))

(defn- change-grid! [pane direction]
  (invoke-later
    #(let [cur-state @state
           cur-state (move-grid cur-state direction)
           cur-state (add-2-to-grid cur-state)]
      (do
        (reset! state cur-state)
        (display-grid pane cur-state)
        (display-score pane cur-state)))))

(defn- on-key-event [^KeyEvent e]
  (let [com (.getSource e)
        key (.getKeyCode e)]
    (condp = key
      KeyEvent/VK_UP
      (change-grid! com :up)
      KeyEvent/VK_DOWN
      (change-grid! com :down)
      KeyEvent/VK_LEFT
      (change-grid! com :left)
      KeyEvent/VK_RIGHT
      (change-grid! com :right)
      nil)))

(defn- create-main-frame [main-pane]
  (let [frm (frame "2048"
                   (stack
                     (label "Score: 0")
                     main-pane) 650 500)]
    frm))

(defn -main [& args]
  (let [grid-pane (grid 4 4 #(label "世界和平"))
        ^JFrame gui (create-main-frame grid-pane)
        _ (bind-key-event grid-pane on-key-event)]
    (invoke-later #(.setVisible gui true))
    (display-grid grid-pane @state)))