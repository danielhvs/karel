(ns karel.core
  (:require
    [quil.core :as q]
    [quil.middleware :as m]))

(def walk-dir
  {0 {:x inc}
   90 {:y inc}
   180 {:x dec}
   270 {:y dec}})

(defn entity [x y]
  {:x x :y y :angle 0})

(defn pos [entity]
  [(:x entity) (:y entity)])

(defn on-chip? [state]
  (let [karel (:karel state)
        k-pos (pos karel)
        cs-pos (map pos (:chips state))]
    (seq (filter #(= k-pos %) cs-pos))))

(defn turn [state]
  (let [karel (:karel state)
        angle (:angle karel)
        next-angle (+ angle 90)
        next-karel
          (assoc karel :angle (mod next-angle 360))]
    (assoc state :karel next-karel)))

(defn step "state is a vector of entities"
  [state]
  (let [karel (:karel state)
        angle (:angle karel)
        dir-fn (get walk-dir angle)
        dir (first (keys dir-fn))
        walk-fn (first (vals dir-fn))]
    (assoc state :karel
        (update karel dir walk-fn))))

(defn make-vertical-line [x y-ini y-end]
  (for [y (range y-ini y-end)]
    (entity x y)))

(defn make-horizontal-line [y x-ini x-end]
  (for [x (range x-ini x-end)]
    (entity x y)))

(def s1
  {:karel (entity 3 3)
   :chips [(entity 1 1) (entity 3 3)]
   :walls
     (concat
       (make-horizontal-line 0 0 20)
       (make-horizontal-line 20 0 20)
       (make-vertical-line 0 0 20)
       (make-vertical-line 20 0 20))})
(map pos
     (:walls s1))

(comment
  {:karel {:x 0 :y 0 :angle 0}
   :chips [{}]}

  (let [state {:karel (entity 1 0)
               :chips [(entity 2 0) (entity 3 0) (entity 1 0)]
               :walls [(entity 2 0) (entity 3 0)]}]
    (on-chip? state))
  (let [state {:karel (entity 1 0)
               :chips [(entity 2 0) (entity 3 0)]}]
    (on-chip? state))
  (:karel {:karel (entity 1 0)
           :walls [(entity 1 0) (entity 1 0)]})

  (step {:karel (entity 0 0)})
  (entity 0 0)
  (turn (turn {:karel (entity 1 0)
               :chips [(entity 0 0)]}))
  (turn
    (step [(entity 1 0) (entity 1 1)])))

(def W 800)
(def H 600)
(def ENTER 10)

(defn setup "returns the initial state" []
  ; Set frame rate frames per second.
  (q/frame-rate 10)
  ; Set color mode to HSB (HSV) instead of default RGB.
  (q/color-mode :hsb)
  ; setup function returns initial state. It contains
  ; circle color and position.
  {:image (q/load-image "resources/test.png")
   :s s1})

(defn key-to-offset-w [key]
  (case (:key key)
    :left dec
    :right inc
    identity))

(defn key-to-offset-h [key]
  (case (:key key)
    :up dec
    :down inc
    identity))

(defn the-key-handler [state k]
  (assoc state
    :s (:s state)
    :done (= ENTER (:key-code k))))

(defn f-all-pos [walls]
  (map (fn [w] (pos w)) walls))

(defn update [state]
  (let [img (:image state)]
    (if (q/loaded? img)
      {:image img
       :s (:s state)
       :done (:done state)}
      {:image img
       :s (:s state)
       :done (:done state)})))

(defn draw-labels [state]
  (let [img (:image state)]
    (when-let [all-pos (f-all-pos (:walls (:s state)))]
      (do
        (println "DAAAAAAAAAAAAAAAAA" all-pos)
        (flush)
        (q/background 255)
        (doall
          (map #(q/image img (first %) (second %)) all-pos))))))

(defn draw [state]
  (when (:all-pos state)
    (if (:done state)
      (do
        (q/do-record (q/create-graphics W H :pdf "out.pdf")
                     (draw-labels state))
        (q/exit))
      (draw-labels state))))

(q/defsketch label-maker
  :title "karel"
  :size [W H]
  ; setup function called only once, during sketch nitialization.
  :setup setup
  ; update is called on each iteration before draw.
  :update update
  :draw draw
  :features [:keep-on-top]
  :key-pressed the-key-handler
  ; This sketch uses functional-mode middleware.
  ; Check quil wiki for more info about middlewares and particularly
  ; fun-mode.
  :middleware [m/fun-mode m/pause-on-error])
