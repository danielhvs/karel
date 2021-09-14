(ns karel.core
  (:require
    [quil.core :as q]
    [quil.middleware :as m]))

(def walk-dir
  {0 {:x inc}
   90 {:y inc}
   180 {:x dec}
   270 {:y dec}})

(defn entity
  ([x y z a]
   {:x x :y y :z z :angle a})
  ([x y]
   (entity x y 0 0)))

(defn pos-xy [entity]
  [(:x entity) (:y entity)])

(defn get-karel [state]
  (first (:karel state)))

(defn on-chip? [state]
  (let [karel (get-karel state)
        k-pos (pos-xy karel)]
    (first (filter #(= k-pos (pos-xy %)) (:chips state)))))

(defn turn [state]
  (let [karel (get-karel state)
        angle (:angle karel)
        next-angle (+ angle 90)
        next-karel
          [(assoc karel :angle (mod next-angle 360))]]
    (assoc state :karel next-karel)))

(defn in-front-of-wall? [state next-karel]
  (seq (filter
         (fn [w] (= (pos-xy next-karel) (pos-xy w)))
         (:walls state))))

(defn chip-handled? [c]
  (> (:z c) 0))

(defn step "state is a vector of entities"
  [state]
  (let [karel (get-karel state)
        angle (:angle karel)
        dir-fn (get walk-dir angle)
        dir (first (keys dir-fn))
        walk-fn (first (vals dir-fn))
        next-karel (update karel dir walk-fn)]
    (if (in-front-of-wall? state next-karel)
      state
      (assoc state
        :karel [next-karel]
        :chips (let [chips (:chips state)]
                 (map (fn [c]
                        (if (chip-handled? c)
                          (assoc c
                            :x (:x next-karel)
                            :y (:y next-karel))
                          c)) chips))))))

(defn make-vertical-line [x y-ini y-end]
  (for [y (range y-ini (inc y-end))]
    (entity x y)))

(defn make-horizontal-line [y x-ini x-end]
  (for [x (range x-ini (inc x-end))]
    (entity x y)))

(defn drop-chip [state]
  (if-let [chip (on-chip? state)]
    (let [chips (:chips state)
          new-chip (assoc chip :z -1)]
      (assoc state :chips
          (conj
            (remove (fn [c] (= chip c)) chips)
            new-chip)))
    state))

(defn grab [state]
  (if-let [chip (on-chip? state)]
    (let [chips (:chips state)
          new-chip (assoc chip :z 1)]
      (assoc state :chips
          (conj
            (remove (fn [c] (= chip c)) chips)
            new-chip)))
    state))

(def s1
  {:karel [(entity 1 1)]
   :chips [(entity 2 1 -1 0)]
   :goals [(entity 6 2)]
   :walls
     (concat
       (make-horizontal-line 0 0 7)
       (make-horizontal-line 3 0 7)
       (make-vertical-line 0 0 3)
       (make-vertical-line 7 0 3))})

(comment
  (grab
    (grab {:karel [(entity 1 1)]
           :chips [(entity 1 1) (entity 2 1)]}))
  (let [state {:karel [(entity 1 0)]
               :chips [(entity 2 0) (entity 3 0) (entity 1 0)]
               :walls [(entity 2 0) (entity 3 0)]}]
    (on-chip? state))
  (let [state {:karel [(entity 1 0)]
               :chips [(entity 2 0) (entity 3 0)]}]
    (on-chip? state))
  (step {:karel [(entity 0 0)]
         :chips [(entity 0 0 -1 0)]})
  (turn (turn {:karel [(entity 1 0)]
               :chips [(entity 0 0)]}))
  (turn
    (step s1)))

(def L 32)
(def W 800)
(def H 600)

(defn point->quil [entity length]
  (assoc entity
    :x (* (:x entity) length)
    :y (* (:y entity) length)))

(defn points->quil [entities length]
  (map (fn [e] (point->quil e length)) entities))

(defn setup "returns the initial state" []
  {:scenario s1
   :karel (q/load-image "resources/head.png")
   :walls (q/load-image "resources/box32.png")
   :chips (q/load-image "resources/circle32.png")
   :goals (q/load-image "resources/square.png")})

(def solution1
  [step grab step step step step turn step drop-chip
   turn turn step turn turn turn step step step step step
   turn turn])

(defn the-key-handler [{:keys [scenario] :as state} k]
  (case (:key-code k)
    83 (assoc state :iteration 0) ; S
    (assoc state :scenario
        (case (:key-code k)
          68 (drop-chip scenario) ; D
          71 (grab scenario) ; G
          74 (turn scenario) ; J
          75 (step scenario) ; K
          scenario))))

(defn update-state [state]
  (if-let [index (:iteration state)]
    (if (< index (count solution1))
      (let [event (nth solution1 index)]
        (assoc state :scenario
            (event (:scenario state))
          :iteration (inc index)))
      state)
    state))

(defn draw-element "{:type [x y]}"
  [{:keys [x y]} img]
  (q/image img x y))

(defn sort-to-draw [scenario]
  (let [res
          (for [kind (keys scenario)]
            (map (fn [e] {:type kind
                          :x (:x e)
                          :y (:y e)
                          :z (:z e)
                          :angle (:angle e)}) (kind scenario)))]
    (->> res
         (reduce #(into %1 %2) [])
         (sort-by :z))))

(defn draw-state [state]
  (q/clear)
  (q/background 255 255 255)
  (let [scenario (-> state :scenario)
        entities (sort-to-draw scenario)]
    (doseq [e (points->quil entities L)]
      (draw-element e ((:type e) state)))))

(q/defsketch karel-sketch
  :title "karel"
  :size [W H]
  ; setup function called only once, during sketch nitialization.
  :setup setup
  ; update is called on each iteration before draw.
  :update update-state
  :draw draw-state
  :features [:keep-on-top]
  :key-pressed the-key-handler
  ; This sketch uses functional-mode middleware.
  ; Check quil wiki for more info about middlewares and particularly
  ; fun-mode.
  :middleware [m/fun-mode m/pause-on-error])
