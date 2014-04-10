(ns gui.core
  (:import
    (javax.swing Box JPanel BoxLayout JSplitPane JButton JTextField JLabel JOptionPane JFrame)
    (java.awt FlowLayout Component GridLayout BorderLayout)
    (java.awt.event ActionListener)))

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

