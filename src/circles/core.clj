(ns circles.core
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]))

;;; Circles Game Basic Setup
;;; CSCI 2041 Homework #7
;;; Spring 2016
(defn left-wall [c]
    "if move to left-wall bounce back"
    (let [h (mod (:heading c) q/TWO-PI)]
      (<= (- (:x c) (/ (:size c) 2)) 0)))
(defn right-wall [c]
    "if move to right-wall bounce back"
    (let [h (mod (:heading c) q/TWO-PI)]
          (>= (+ (:x c)(/ (:size c) 2)) 500)))
(defn up-wall [c]
    "if move to up-wall bounce back"
    (let [h (mod (:heading c) q/TWO-PI)]
          (<= (- (:y c) (/ (:size c) 2)) 0)))
(defn down-wall [c]
    "if move to left-wall bounce back"
    (let [h (mod (:heading c) q/TWO-PI)]
          (>= (+ (:y c) (/ (:size c) 2)) 500)))

;this implementation is buggy
(defn collide? [circle-1 circle-2]
  (and (= (:x circle-1) (:x circle-2))
       (= (:y circle-2) (:y circle-2))
       )
  )
;;; Constants
(def speed 6)                          ;maximm speed circles move

;---------------------------------------------------------------------
; Setup
;---------------------------------------------------------------------
(defn make-rectangle [x y]
  (let [angle (rand q/TWO-PI)
        cur-speed 4
        ]
    {
     :x x
     :y y
     :size (+ 20 (rand 30))
     :color (rand 255)
     :speed cur-speed
     :heading angle
       }
))

(defn make-circle
  "Creates a circle with a random color and set speed and heading."
   [x y]
  (let [angle (rand q/TWO-PI)          ;random angle
        cur-speed (+ (rand speed) 1)]  ;random speed up to our constant
       {:x x                           ;set this circle's x
    	:y y                           ;set this circle's y
        :size (+ 10 (rand 15))         ;set random diameter
    	:color (rand 255)              ;make this colorful
    	:speed cur-speed               ;set this circle's speed
    	:heading angle}                ;set this circle's heading
    ))                                 ;returns circle

(defn setup
  "Set up a sketch and return initial state."
  []
  (q/frame-rate 30)                    ;frequency update and draw functions
  (q/color-mode :hsb)                  ;how we represent colors
  (let [size (q/width)
        n 20
        bg 250]
       (q/background bg)               ;nice light grey color for the bg
       ;; need to make n circles of random sizes
       ;; here we make only one circle in a list
       {
        :circles (list  (make-circle 100 100)(make-circle 100 100)
                        (make-circle 100 100)(make-circle 100 100)
                        (make-circle 100 100)(make-circle 100 100)
                        (make-circle 100 100)(make-circle 100 100)
                        (make-circle 100 100)(make-circle 100 100)
                        (make-circle 100 100)(make-circle 100 100)
                        (make-circle 100 100)(make-circle 100 100)
                        (make-circle 100 100)(make-circle 100 100)
                        (make-circle 100 100)(make-circle 100 100)
                        (make-circle 100 100)(make-circle 100 100)
                        (make-circle 100 100)(make-circle 100 100)
                        (make-circle 100 100)(make-circle 100 100)
                                        	)
        :running? true                 ;so we can pause and unpause in update
       ; :x	250							;x coordinate
       ; :y	250							;y coordinate
        :n n                           ;how many circles
        :size size                     ;how big is the sketch
        :bg bg                         ;we might want to change this later
        :rectangle  (make-circle 250 250)  ;the player ball
        :scores (quot (System/currentTimeMillis) 1000)
        }
       ))

;---------------------------------------------------------------------
; Update functions
;---------------------------------------------------------------------

(defn bounce-back [c size]
  (let [ x (:x c)
         y (:y c)
         r (/ (:size c) 2)
         nc (if (left-wall c)
              (update-in c [:heading] (fn [n] (- n q/HALF-PI)))
              c)]
    (cond (right-wall nc) (update-in nc [:heading] (fn [n] (- n q/HALF-PI)))
          (up-wall nc) (update-in nc [:heading] (fn [n] (- n)))
          (down-wall nc) (update-in nc [:heading] (fn [n] (- n)))
          :else nc
      )))


(defn move-circle [c state]
  (q/text (str "ScoreBoard:  " [(- (quot (System/currentTimeMillis) 1000) (:scores state))] )  0 10)
  (let [angle (:heading (bounce-back c (:size c)))
        dx (* (:speed c) (q/cos angle))
        dy (* (:speed c) (q/sin angle))]
  	(update-in (update-in (bounce-back c (:size c)) [:x] (fn [n] (+ n dx)))
               [:y] (fn [n] (+ n dy)))
    ))


(defn update-circles
  "Moves each circle and returns updated vector of circles."
  [circles state]
  (map (fn [c] (move-circle c state)) circles)
  ;(check collasion)
  )


;(collasion )

"Updates sketch state. If it is paused, then the state is returned unmodified."
(defn update-state  [state]
  (if (:running? state)
      ;add some movement and update functions so the next line moves circles
      (assoc state :circles (update-circles (:circles state) state))
      state))

;---------------------------------------------------------------------
; Draw functions
;---------------------------------------------------------------------

(defn draw-circle [c]
  "Draws an individual circle with correct color, location, and size."
  (q/fill (:color c) 255 255)
  (q/ellipse (:x c) (:y c) (:size c) (:size c))
  )

(defn draw-rectangle
  "Draws an individual circle with correct color, location, and size."
  [rec]
  (q/fill (:color rec) 255 255)
  ;(q/ellipse (:x c) (:y c) (:size c) (:size c))
  (q/rect (:x rec) (:y rec) (:size rec) (:size rec))
  )

(defn draw-state
  "Draws the sketch state."
  [state]
  (q/background (:bg state))                    ;update the background
  (q/stroke 1)   ;how wide should the lines be
  (dorun (draw-rectangle (:rectangle state)))
  (dorun (map draw-circle (:circles state)))    ;map is lazy
  (q/fill 0)
  )

;---------------------------------------------------------------------
; User interaction functions
;---------------------------------------------------------------------

(defn mouse-clicked
  "Changes background color to different shades of grey."
  [state event]
  (update-in state [:bg] (fn [n] (rand-int 255))))

(defn key-pressed
  "Process key event.  p will pause/unpause everything."
  [state event]
  (condp = (:key event)
    :p (update-in state [:running?] not)
    :up (update-in state [:rectangle :y] (fn [x] (- x 10)))
    :down (update-in state [:rectangle :y] (fn [x] (+ x 10)))
    :left (update-in state [:rectangle :x] (fn [x] (- x 10)))
    :right (update-in state [:rectangle :x] (fn [x] (+ x 10)))
    state))

(q/defsketch circles
    :host "host"
    :size [500 500]                ;we need a square canvas
    :setup setup                   ;getting things started, setting initial state
    :update update-state           ;the function to update the state
    :draw draw-state               ;the necessary draw function
    :mouse-clicked mouse-clicked   ;this is our mouse click event
    :key-pressed key-pressed       ;this is our keyboard input event
    :middleware [m/fun-mode])      ;this gives us the ability to have state

