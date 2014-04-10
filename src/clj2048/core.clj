(ns clj2048.core
  (:gen-class)
  (:use [gui.core]))

(defn -main [& args]
  (let [gui (frame "hello"
                   (splitter
                     (grid 10 10 #(label "世界和平"))
                     (button "click me" #(alert "hi"))) 650 500)]
    (.setVisible gui true)))