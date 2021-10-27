
(ns paint.core
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [clojure.java.io :as io :only file]
            [clojure.edn :as edn :only read-string]
            [clojure.math.numeric-tower :as math :only abs]))

(defn run [] (use 'paint.core :reload-all))
(defn get-mouse-loc []
  [(q/mouse-x) (q/mouse-y)])

(defn line [state]
  (let [m-loc (:mouse-loc-data state)
        b-rel (:m-button-release state)
        color (:draw-color state)
        draw-width (:draw-width state)
        [x y] (get-mouse-loc)
        button-pressed (:m-button-pressed state)
        {{:keys [min-x max-x min-y max-y]} :bounding-box} state]
    (cond
      (<= 2 (count (peek m-loc))) (assoc state :mouse-loc-data (conj (:mouse-loc-data state) []))
      (and  b-rel
            (not (and  (<= min-x  x max-x) (<= min-y y max-y)))
            (not= [x y] (take-last 2 (peek (peek m-loc)))));checks for dupes
      (assoc state :mouse-loc-data (conj (pop m-loc) (conj (peek m-loc) [color draw-width x y])) :m-button-release false)
      :else (assoc state :m-button-release false))))

(defn drawing [state]
  (let [mouse-loc-data (:mouse-loc-data state)
        draw-color (:draw-color state)
        draw-width (:draw-width state)
        [x y] (get-mouse-loc)
        button-pressed (:m-button-pressed state)
        {{:keys [min-x max-x min-y max-y]} :bounding-box} state]

    (cond
      (and (not button-pressed) (seq (peek mouse-loc-data))) (assoc state :mouse-loc-data (conj mouse-loc-data []));adds [] to the end to seprate lines drawn
      (and button-pressed
           (not (and  (<= min-x x max-x) (<= min-y y max-y))); stops adding when clicking on buttons
           (not= [x y]  (take-last 2 (peek (peek mouse-loc-data))))) ; removes dupes
      (assoc state :mouse-loc-data (conj (pop mouse-loc-data) (conj (peek mouse-loc-data) [draw-color draw-width x y])))
      :else state)))

(defn save [state]
  (when-not (state :file-saved) (spit "test.txt" (state :mouse-loc-data)))
  true)

(defn loading [state]
  (if (.exists (io/file "test.txt"))
    (let [color (:color (state :s-button-selected))]
      (if (not (state :file-loaded))
        (assoc state :mouse-loc-data (edn/read-string (slurp "test.txt"))
               :file-loaded true :s-button-selected {:func nil :color color}) state))
    state))

(def buttons ; add buttons here
  [{:name "line" :color false} {:name "draw" :color false}
   {:name "save" :color false} {:name "load" :color false}
   {:name "undo" :color false} {:name "redo" :color false}
   {:name "test2" :color false}

   {:name "red"  :color -65536}
   {:name "green" :color -16711936} {:name "blue" :color -16776961}
   {:name "black" :color -16777216} {:name "white" :color -1}])

(def height 40) ;button height
(def width 95) ;button width
(def x 10)
(def y 10)

(defn setup []
  (q/frame-rate 60)
  ; setup function returns initial state.
  (let [button-loc-y (mapv #(into [] [% (+ % (- height 1))]) (range y (+ (* (count buttons) height) y) height)) ; avoids overlapping buttons
        button-loc-x [x (+ width x)]
        bounding-box {:min-x (first button-loc-x) :max-x (last button-loc-x) :min-y (first (first button-loc-y)) :max-y (last (last button-loc-y))}]
    {:file-saved false
     :file-loaded false

     :toggle-buttons true
     :button-names (mapv #(:name %) buttons)
     :color-names {"red" -65536 "green" -16711936 "blue" -16776961 "black" -16777216 "white" -1}
     :button-loc  (mapv #(vector x % width height) (range  y (+ (* (count buttons) height) y) height)) ;format is [x y width height]
     :button-loc-y button-loc-y ;(mapv #(into [] [% (+ % (- height 1))]) (range 0 (* (count buttons) height) height)) ; avoids overlapping buttons
     :button-loc-x button-loc-x
     :bounding-box bounding-box

     :s-button-selected {:func nil :color "black"}

     :m-button-release false
     :m-button-pressed false

     :k-button-released false
     :k-button-pressed false

     :mouse-loc-data  [[]]
     :loc-data-redo [[]]

     :current-mouse-loc nil
     :draw-color 0
     :draw-width 15}))

(defn which-button [x y button-state button-loc-y button-names bounding-box toggle-buttons]
  (let [{:keys [min-x max-x min-y max-y]} bounding-box]
    (if (and  toggle-buttons (<= min-x  x max-x) (<= min-y y max-y))
      (let [choice
            (first
             (keep-indexed
              (fn [index item]
                (let [[a b] item]
                  (when (<= a y b)
                    index))) button-loc-y))]
        (if (:color (buttons choice))
          (assoc button-state :color (button-names choice))
          (assoc button-state :func (button-names choice))))
      button-state)))

(defn undo [state undo]
  (let [ld-redo (:loc-data-redo state)
        m-loc-data (:mouse-loc-data state)]
    (if undo
	;m-loc-data -> ld-redo
      (if (not-empty (pop m-loc-data))
        (assoc state :loc-data-redo    (conj (pop  ld-redo) (peek (pop  m-loc-data)) [])
               :mouse-loc-data (conj (pop (pop m-loc-data)) [])
               :s-button-selected {:func nil :color (:color (state :s-button-selected))})
        (assoc state :s-button-selected {:func nil :color (:color (state :s-button-selected))}))
      ;ld-redo -> m-loc-data
      (if (not-empty (pop ld-redo))
        (assoc state :loc-data-redo (conj (pop (pop ld-redo)) [])
               :mouse-loc-data (conj (pop m-loc-data) (peek (pop ld-redo)) [])
               :s-button-selected {:func nil :color (:color (state :s-button-selected))})
        (assoc state :s-button-selected {:func nil :color (:color (state :s-button-selected))})))))

(defn update-state [state]
  (let [func  (:func  (:s-button-selected state))]
    (cond-> state
      (= func  "undo") (undo true)  ;undo
      (= func  "redo") (undo false) ;redo
;clears screen 
      (and (q/key-pressed?) (= (q/key-as-keyword) :c)) (assoc :mouse-loc-data [[]])
	;toggles buttons displaying  
      (and (:k-button-released state) (= (q/key-as-keyword) :space)) (assoc  :toggle-buttons (not (:toggle-buttons state)))
      (:k-button-released state) (assoc :k-button-released false);needs to be after keyboard key presses to make sure it only gets activated once per key press
    ;draw
      (= func "draw") (drawing)
    ;line
      (= func "line") (line)
    ;load
      (= func "load") (loading)
      (not= func "load") (assoc :file-loaded false)
    ;save
      (and (= func "save") (not (:file-saved state))) (assoc :file-saved (save state))
      (not= func "save") (assoc :file-saved false))))

(defn draw-button [x y width height b-name b-pressed]
  (q/stroke 0)
  (q/stroke-weight 1)
  (if b-pressed (q/fill 0 255 200) (q/fill 0 0 200))
  (q/rect x y width height 1  1 10 1)
  (q/fill 40 90 200)
  (q/rect (+ x 4) (+ y 4) (- width 8) (- height  8) 1  1 10 1)
  (q/fill 255 255 255)
  (q/text-align :left :top)
  (q/text b-name  (+ x 10)  y))

(defn draw-screen [state]
  (q/background 200)
  (q/text-size 30)
  (q/stroke-weight 1)
  (q/text-align :center :center)
  (when (q/key-pressed?) (q/text (str "key as keyword:" (q/key-as-keyword)) (q/mouse-x) (+ (q/mouse-y) 30)))
  (when (q/mouse-pressed?) (q/text (str (q/mouse-x) " " (q/mouse-y)) (q/mouse-x) (+ (q/mouse-y) 10)))

  (when (seq (:mouse-loc-data state)); needs to enventual be removed
    (dorun (map
            #(cond
               (= 0 (count %)) nil
               (= 1 (count %)) ((fn [[[color width x y]]] (q/stroke color) (q/fill color) (q/stroke-weight 0) (q/ellipse x y  width width)) %) ;individual pixels are really small :p
               (< 1 (count %)) (doall (map (fn [[color width x1 y1 - -  x2 y2]] (q/stroke color) (q/stroke-weight width) (q/line x1 y1 x2 y2)) (partition 8 4 (flatten %))))) (:mouse-loc-data state))))

  (q/stroke (state :draw-color))
  (q/fill (state :draw-color))
  (q/stroke-weight 0)
  (q/ellipse (q/mouse-x) (q/mouse-y)  (:draw-width state) (:draw-width state))

  (when (:toggle-buttons state)
    (dorun  ;function buttons
     (map (fn [[x y width height] button-names] (draw-button  x y width height button-names
    ; this allows for 2 buttons to be highlighted a func and color button
                                                              (or (= (:func (:s-button-selected state)) button-names)
                                                                  (= (:color (:s-button-selected state)) button-names))))
          (:button-loc state) (:button-names state))))
  state)

(defn m-pressed [state m-state]
  (let [b-clicked (which-button (m-state :x) (m-state :y) (:s-button-selected state) (:button-loc-y state) (:button-names state) (:bounding-box state) (:toggle-buttons state))
        c-clicked (:color b-clicked)]
    (assoc state :m-button-pressed true :s-button-selected b-clicked :draw-color ((:color-names state) c-clicked))))

(defn wheel [state w-state]
  (let [num (math/abs (+ (:draw-width state) w-state))] ;make sure :draw-width doesnt go negative
    (assoc state :draw-width num)))

(defn m-release [state r-state]
  (let [x (:x r-state)
        y (:y r-state)
        {{:keys [min-x max-x min-y max-y]} :bounding-box} state]
    (if (not (and (<= min-x  x max-x) (<= min-y y max-y)))
      (assoc state :m-button-release true :m-button-pressed false)
      (assoc state :m-button-release true :m-button-pressed false))))

(defn k-pressed [state k-state] (assoc state :k-button-pressed true :k-button-released false))
(defn k-released [state k-state] (assoc state :k-button-pressed false :k-button-released true))

(q/defsketch paint
  :title "draw: hit C to clear screen : use scroll wheel to resize paintbrush"
  :size [500 500]
  :setup setup
  :update update-state
  :draw draw-screen
  :key-pressed k-pressed
  :key-released k-released
  :mouse-pressed m-pressed
  :mouse-released m-release
  :mouse-wheel wheel
  :features [:resizable]
  :middleware [m/fun-mode])
