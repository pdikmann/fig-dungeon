(ns fig-dungeon.common)

(def gridsize 7)

(defn random [n]
  (.round js/Math (* n (.random js/Math))))

(defn random-dir []
  (nth [:up :down :left :right] (random 3)))

(defn at-same-position? [{x1 :x y1 :y}
                         {x2 :x y2 :y}]
  (and (= x1 x2)
       (= y1 y2)))

(defn tile-index [x y]
  (+ x (* y gridsize)))

(defn out-of-bounds?
  ([{:keys [x y]}]
   (out-of-bounds? x y))
  ([x y]
   (or (< (min x y) 0)
       (> (max x y) (dec gridsize)))))
