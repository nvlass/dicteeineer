(ns dicteeineer.core
  (:require [reagent.dom.client :as rdc]
            [re-frame.core :as rf]
            [re-frame.db :refer [app-db]]
            [clojure.string :as str]
            [dicteeineer.events]
            [dicteeineer.subs]
            [dicteeineer.views :as views]
            [dicteeineer.audio :as audio]
            [dicteeineer.storage :as storage]))

(defonce root (atom nil))

(defn ^:dev/after-load mount []
  (rf/clear-subscription-cache!)
  (when-not @root
    (reset! root (rdc/create-root (js/document.getElementById "app"))))
  (rdc/render @root [views/app]))

(def captured #{"ArrowUp" "ArrowDown" "ArrowLeft" "ArrowRight" " " "Backspace"})

(defn- on-key [e]
  (let [tag (some-> (.-target e) .-tagName str/lower-case)
        k (.-key e)]
    (when-not (or (#{"input" "select" "textarea"} tag) (.-altKey e) (.-metaKey e) (.-ctrlKey e)
                  (and (= tag "button") (#{" " "Enter"} k)) (.-repeat e))
      (when (and (captured k) (#{:note :dictation} (:view @app-db))) (.preventDefault e))
      (rf/dispatch [:key k]))))

(defn init []
  (rf/dispatch-sync [:init])
  (storage/load-all! [:settings :attempts :history :dict] #(rf/dispatch [:storage/loaded %]))
  (audio/install-unlock!)
  (audio/load! #(rf/dispatch [:audio/loaded %]))
  (.addEventListener js/document "keydown" on-key)
  (.addEventListener js/window "resize" #(rf/dispatch [:resize (.-innerWidth js/window)]))
  (when (and (not ^boolean goog.DEBUG) (.-serviceWorker js/navigator))
    (.register (.-serviceWorker js/navigator) "sw.js"))
  (mount))
