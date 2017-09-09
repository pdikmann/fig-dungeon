;; ideas:
;; - buy upgrades with energy: more energy per enemy drop, faster movement (e.g. frequent enemy pause), more enemy spawns (less cooldown between spawns)
;; - increase difficulty by removing floors (so player can't move on it, enemies die on it)
;; - establish 'drop zones', e.g. enemies caught near center are worth less (drop less energy) than enemies caught around the edge of the map

(ns fig-dungeon.core
  (:require [reagent.core :as reagent :refer [atom]]
            [fig-dungeon.common :refer [random-dir
                                        random
                                        at-same-position?
                                        out-of-bounds?]]))

(enable-console-print!)

(def initial-state {:enemies []
                    ;;:id-counter 0
                    :moves 0
                    :player {;;:active false
                             :energy 10
                             :x 4 :y 4}})

(defonce app-state (atom initial-state))

(def counter 0)

(defn my-gensym []
  (set! counter (inc counter)))

(defn init []
  (reset! app-state initial-state))

(defn move-enemy [e]
  (if (:dead e)
    e
    (case (:dir e)
      :up (update e :y dec)
      :down (update e :y inc)
      :left (update e :x dec)
      :right (update e :x inc))))

(defn move-enemies []
  (swap! app-state
         update :enemies
         #(mapv #'move-enemy %)))

(defn create-enemy []
  (let [dir (random-dir)
        pos (random 8)
        nme (case dir
              :up    {:x pos :y 8}
              :down  {:x pos :y 0}
              :left  {:x 8 :y pos}
              :right {:x 0 :y pos})]
    (if (at-same-position? nme (:player @app-state))
      (recur)
      (assoc nme
             :dir dir
             :id (my-gensym)
             :dead false))))

(defn spawn-enemy []
  (when (= 0 (mod (:moves @app-state) 3))
    (swap! app-state
           update :enemies
           #(conj % (create-enemy)))))

(defn collect-enemy []
  (let [player-pos (:player @app-state)
        at-player-position? #(at-same-position? % player-pos)
        collection (filter at-player-position? (:enemies @app-state))
        ]
    (when (not (empty? collection))
      (swap! app-state
             (fn [s]
               (-> s
                   (update-in [:player :energy] #(+ % (* 4 (count collection))))
                   (update :enemies (fn [es]
                                      (mapv (fn [e]
                                              (if (at-player-position? e)
                                                (assoc e :dead true)
                                                e))
                                            es)))
                   )))
      (println "nmes" (:enemies @app-state)))))

(defn remove-dead-enemies []
  (swap! app-state
         update :enemies
         (fn [es] (vec (remove #(:dead %) es)))))

(defn remove-runaway-enemies []
  (swap!
   app-state
   update :enemies
   (fn [es]
     (mapv #(if (out-of-bounds? %)
              (assoc % :dead true)
              %)
           es))))

(defn opponents-move []
  "called whenever the player moves"
  (remove-dead-enemies)
  (collect-enemy)
  (move-enemies)
  (collect-enemy)
  (spawn-enemy)
  (remove-runaway-enemies))

;; (defn toggle-player []
;;   (swap!
;;    app-state
;;    update-in [:player :active] not))

(defn move-player 
  ([dir]
   (let [{:keys [x y]} (:player @app-state)]
     (case dir
       :up (move-player x (dec y))
       :down (move-player x (inc y))
       :left (move-player (dec x) y)
       :right (move-player (inc x) y))))
  ([x y]
   (when (and ;;(-> @app-state :player :active)
          (< 0 (-> @app-state :player :energy))
          (not (out-of-bounds? {:x x :y y})))
     ;;(collect-enemy)
     (swap!
      app-state
      #(-> %
           (update :player assoc :x x :y y)
           (update-in [:player :energy] dec)
           (update :moves inc)))
     (opponents-move))))

;; (defn activate-player []
;;   (when (not (:active (:player @app-state)))
;;     (swap! app-state assoc-in [:player :active] true)))

(defn position-style [thing]
  {:top (* 50 (:y thing))
   :left (* 50 (:x thing))})

(defn key-down [e]
  (case (.-key e)
    "r" (init)
    "w" (move-player :up)
    "a" (move-player :left)
    "s" (move-player :down)
    "d" (move-player :right)
    nil))

(defn hello-world []
  [:div.main 
   [:div.field
    {:tab-index 1
     :on-key-down #'key-down}
    ;; Player
    [:div.player
     {:style (position-style (:player @app-state))
      ;;:on-click #'toggle-player
      }]
    ;; Enemies
    (doall
     (for [e (:enemies @app-state)]
       ^{:key (:id e)}
       [:div.enemy {:style (position-style e)
                    :class (if (:dead e)
                             "dead"
                             nil)}
        ;;(str (:dir e))
        ]))
    ;; Grid
    (for [y (range 9)
          x (range 9)]
      ^{:key (+ x (* y 9))}
      [:div.block
       ;;{:on-mouse-enter (fn [] (move-player x y))}
       ])]
   
   [:br {:style {:clear "both"}}]
   [:div.energy
    {:style {:width (* 20 (-> @app-state :player :energy))}}]
   [:p
    "energy:" (str (-> @app-state :player :energy)) [:br]
    "moves:" (str (:moves @app-state)) [:br]
    ;;"gensym:" (str (:id-counter @app-state))
    ]
   ])

(reagent/render-component [hello-world]
                          (. js/document (getElementById "app")))

(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
)
