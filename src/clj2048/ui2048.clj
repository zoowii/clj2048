(ns clj2048.ui2048
  (:import (javax.swing JPanel JLabel ImageIcon BorderFactory SwingConstants)
           (java.awt.event KeyEvent)
           (java.awt Color Font BorderLayout Graphics Toolkit)
           (javax.swing.border Border))
  (:require [clojure.core.matrix :as matrix])
  (:use gui.core))

(def background-color (color 187 173 160))

(def grid-foreground-color (color 172 122 76))

(defn grid-item [text state]
  ;; 2048的单一格子
  (let [label (doto (JLabel. text)
                (.setFont (Font. nil Font/BOLD 36))
                (.setForeground grid-foreground-color)
                (.setHorizontalAlignment SwingConstants/CENTER))
        pane (doto (proxy [JPanel] []
                     (paintComponent [g]
                       (proxy-super paintComponent g)
                       (draw-image! g "img/empty.png")))
               (.setLayout (BorderLayout.))
               (.setSize 60 60)
               (.setBorder
                 (BorderFactory/createLineBorder
                   background-color 8))
               (.setBackground (Color. 238 228 218))
               (.add label BorderLayout/CENTER))]
    pane))


(defn update-grid-item-text! [pane val]
  (cond
    (< val 1)
    (.setText (.getComponent pane 0) "")
    :else
    (.setText (.getComponent pane 0) (str val))))