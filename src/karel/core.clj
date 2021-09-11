(ns karel.core
  (:require
    [clojure.string :as str]))

(def walk-dir
  {0 {:x inc}
   90 {:y inc}
   180 {:x dec}
   270 {:y dec}})

(defn entity [x y t]
  {:x x :y y :type t :angle 0})

(defn karel [x y]
  (entity x y :karel))

(defn wall [x y]
  (entity x y :wall))

(defn walk [{:keys [angle] :as entity}]
  (let [dir-fn (get walk-dir angle)
        dir (first (keys dir-fn))
        walk-fn (first (vals dir-fn))]
    (update entity dir walk-fn)))

(defn turn [{:keys [angle] :as entity}]
  (let [next-angle (+ angle 90)]
    (assoc entity :angle (mod next-angle 360))))

(defn step "state is a vector of entities"
  [state]
  (if-let [karel (first
                   (filter
                     #(= (get-in % [:type]) :karel)
                     state))]
    (walk karel)
    state))

(comment
  (step [(wall 0 0)])
  (for [x (range 0 700 30)]
    (mod x 360))
  (turn (turn (karel 1 0)))
  (turn
    (step [(karel 1 0) (wall 1 1)])))
