(ns clj2048.core
  (:gen-class)
  (:import (javax.swing JPanel SwingUtilities JLabel JFrame)
           (java.awt.event KeyEvent))
  (:require [clojure.core.matrix :as matrix]
            [clojure.core.matrix.operators :as m-op]
            [clj2048.ui2048 :as ui2048])
  (:use gui.core
        clj2048.util))

(def state (atom {}))

;;; 把grid转成各个方向的rows
(defmulti get-rows-of-direction :direction)

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
;;; 把各个方向的rows转成grid
(defmulti parse-direction-rows-to-grid :direction)

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
  "判断是否无路可走,或者是否达到目标分数，也就是游戏是否结束了"
  (cond
    (>= (:score state) 2048)
    :success
    (empty?
      (filter-matrix #(< % 1) (:grid state)))
    :fail
    :else
    :go-on))

(defn display-grid [panel state]
  ;; 在panel中显示grid
  (let [coms (.getComponents panel)
        grid (:grid state)]
    (doseq [[idx val] (iter-matrix grid)]
      (ui2048/update-grid-item-text! (nth coms idx) val))))

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

(defn- reset-grid! []
  (reset! state
          (-> (assoc @state
                :score 0
                :grid (zero-int-matrix 4))
              add-2-to-grid
              add-2-to-grid)))

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
      nil)
    (condp = (ended-grid? @state)
      :success
      (do
        (alert "Successfully!")
        (reset-grid!))
      :fail
      (do
        (alert "You failed the game!")
        (reset-grid!))
      nil)))

(defn- create-main-frame [main-pane]
  (doto (frame "2048 Author: zoowii"
               (stack
                 (label "Score: 0")
                 main-pane) 600 450)
    (.setBackground ui2048/background-color)))

(defn -main [& args]
  (let [grid-pane (grid 4 4 #(ui2048/grid-item "世界和平" :empty))
        ^JFrame gui (doto (create-main-frame grid-pane)
                      (.setAlwaysOnTop true))
        _ (bind-key-event grid-pane on-key-event)]
    (reset-grid!)
    (invoke-later #(.setVisible gui true))
    (display-grid grid-pane @state)))