(ns fig-dungeon.core
    (:require [reagent.core :as reagent :refer [atom]]))

(enable-console-print!)

(def initial-state {:enemies []
                    :player {:active false
                             :x 4 :y 4}})

(defonce app-state (atom initial-state))

(defn create-enemy []
  {:x 0 :y 0
   :dir :down ; one of [up down left right]
   })

(defn move-enemy [e]
  (case (:dir e)
    :up (update e :y dec)
    :down (update e :y inc)
    :left (update e :x dec)
    :right (update e :x inc)))

(defn toggle-player []
  (swap! app-state
         update-in [:player :active] not))

(defn move-player [x y]
  (when (:active (:player @app-state))
    (swap! app-state
           update :player
           assoc :x x :y y))
)

(defn activate-player []
  (when (not (:active (:player @app-state)))
    (swap! app-state assoc-in [:player :active] true)))

(defn init []
  (reset! app-state initial-state))

(defn hello-world []
  [:div.main {:on-key-down (fn [e] (println "keypress:" (:key e)))}
   [:div.field
    (for [y (range 9)
          x (range 9)]
      ^{:key (+ x (* y 9))}
      [:div.block
       {:on-mouse-enter (fn [] (move-player x y))}
       ;(str x "," y)
       ])
    [:div.player
     {:style {:top (* 50 (:y (:player @app-state)))
              :left (* 50 (:x (:player @app-state)))}
      :on-click #'toggle-player}]]
   ])

(reagent/render-component [hello-world]
                          (. js/document (getElementById "app")))

(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
)
