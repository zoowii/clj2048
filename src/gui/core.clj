(ns gui.core
  (:import
    (javax.swing Box JPanel BoxLayout JSplitPane JButton JTextField JLabel JOptionPane JFrame SwingUtilities ImageIcon)
    (java.awt FlowLayout Component GridLayout BorderLayout Toolkit Graphics Color)
    (java.awt.event ActionListener KeyListener KeyEvent))
  (:require [clojure.java.io :as io]))

(defn get-image-icon [path]
  (ImageIcon. (io/resource path)))

(defn get-image [path]
  (.createImage (Toolkit/getDefaultToolkit) (io/resource path)))

(defn set-icon! [label img-path]
  (.setIcon label (get-image-icon img-path)))

(defn draw-image! [^Graphics g img-path]
  (.drawImage g (get-image img-path) 0 0 nil))

(defn color
  ([r g b]
   (Color. r g b))
  ([color-str]
   (condp = color-str
     :white Color/WHITE
     :black Color/BLACK
     :red Color/RED
     :green Color/GREEN
     :blue Color/BLUE
     :orange Color/ORANGE
     :yellow Color/YELLOW
     Color/BLACK)))

(defn shelf [& components]
  (let [shelf (JPanel.)]
    (.setLayout shelf (FlowLayout.))
    (doseq [c components] (.add shelf c))
    shelf))

(defn stack [& components]
  (let [stack (Box. BoxLayout/PAGE_AXIS)]
    (doseq [c components]
      (.setAlignmentX c Component/CENTER_ALIGNMENT)
      (.add stack c))
    stack))

(defn splitter [top bottom]
  (doto (JSplitPane.)
    (.setOrientation JSplitPane/VERTICAL_SPLIT)
    (.setLeftComponent top)
    (.setRightComponent bottom)))

(defn button [text f]
  (doto (JButton. text)
    (.addActionListener
      (proxy [ActionListener] []
        (actionPerformed [_] (f))))))

(defn text [cols t]
  (doto (JTextField.)
    (.setColumns cols)
    (.setText t)))

(defn label [txt]
  (JLabel. txt))

(defn alert
  ([msg] (alert nil msg))
  ([frame msg]
   (JOptionPane/showMessageDialog frame msg)))

(defn grid [x y f]
  (let [g (doto (JPanel.)
            (.setLayout (GridLayout. x y)))]
    (dotimes [i x]
      (dotimes [j y]
        (.add g (f))))
    g))

(defn horizontal [a b c]
  (let [g (doto (JPanel.)
            (.setLayout (BorderLayout.))
            (.add a BorderLayout/WEST)
            (.add b BorderLayout/CENTER)
            (.add c BorderLayout/EAST))]
    g))

(defn vertical [a b c]
  (let [g (doto (JPanel.)
            (.setLayout (BorderLayout.))
            (.add a BorderLayout/NORTH)
            (.add b BorderLayout/CENTER)
            (.add c BorderLayout/SOUTH))]
    g))

(defn frame
  ([title pane] (frame title pane 0 0 true))
  ([title pane width height] (frame title pane width height nil))
  ([title pane width height pack]
   (let [frame (JFrame. title)]
     (doto frame
       (-> .getContentPane .removeAll)
       (.setLocationRelativeTo nil)
       (.setContentPane pane)
       )
     (if pack
       (.pack frame)
       (.setSize frame width height))
     frame)))

(defn bind-key-event [component handler]
  ;; 绑定键盘事件
  (.setFocusable component true)
  (.addKeyListener
    component
    (proxy [KeyListener] []
      (keyPressed [^KeyEvent e]
        (handler e))
      (keyReleased [_] _)
      (keyTyped [_] _))))

(defn invoke-later [f]
  (SwingUtilities/invokeLater
    (proxy [Runnable] []
      (run []
        (f)))))

