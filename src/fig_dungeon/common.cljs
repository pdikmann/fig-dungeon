(ns fig-dungeon.common)

(defn random [n]
  (.round js/Math (* n (.random js/Math))))

(defn random-dir []
  (nth [:up :down :left :right] (random 3)))

;; (defn at-player-position? [x]
;;   (let [p (:player @app-state)]
;;     (and (= (:x x) (:x p))
;;          (= (:y x) (:y p)))))

(defn at-same-position? [{x1 :x y1 :y}
                         {x2 :x y2 :y}]
  ;;[a b]
  ;; (and (= (:x a) (:x b))
  ;;      (= (:y a) (:y b)))
  (and (= x1 x2)
       (= y1 y2))
  )

(defn out-of-bounds? [{:keys [x y]}]
  (or (< (min x y) 0)
      (> (max x y) 8)))
