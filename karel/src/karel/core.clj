(ns karel.core
  (:require [clojure.string :as str])
  (:gen-class))

(defn elemento->grafico [{:keys [tipo]}]
  (or
   (tipo {:parede "."
          :karel ">"
          :chip "c"})
   " "))

(defn criador-elemento [tipo x y]
  {:x x :y y :tipo tipo})

(def parede (partial criador-elemento :parede))
(def karel (partial criador-elemento :karel))
(def nada (partial criador-elemento :nada))
(def chip (partial criador-elemento :chip))

(defn mundo [tam]
  (for [x (range tam)
        y (range tam)]
    (parede x y)))

(defn coloca [mundo elemento]
  (conj 
   (remove (fn [e] (and (= (:x elemento) (:x e)
                           (:y elemento) (:y e))))
           mundo)
   elemento))

(defn desenha-linha [elementos]
  (->
   (map elemento->grafico elementos)
   (str/join)
   (str "\n")))

(defn desenha [mundo]
  (let [max-x (->> (map :x mundo) 
                 (apply max)
                 inc)]
    (map desenha-linha (partition max-x 
                                  (sort-by :y mundo)))))

(def cenario (-> (mundo 5)
                 (coloca (karel 0 0))
                 (coloca (chip 1 2))
                 (coloca (chip 2 4))))

(defn anda 
  ([karel] 
   (update karel :x inc))
  ([karel n]
   (last (take n (iterate anda karel)))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println (desenha cenario)))
