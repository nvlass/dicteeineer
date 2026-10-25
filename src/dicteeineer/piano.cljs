(ns dicteeineer.piano
  (:require [reagent.core :as r]
            [dicteeineer.theory :as t]))

(defn- white-lo [m] (if (t/white? m) m (dec m)))
(defn- white-hi [m] (if (t/white? m) m (inc m)))

(defn piano
  "Playable keyboard over [lo, hi]; `marks` maps MIDI notes to a class, `label` names a C key."
  [_]
  (let [down (r/atom #{})
        center! (fn [el] (when el (set! (.-scrollLeft el) (/ (- (.-scrollWidth el) (.-clientWidth el)) 2))))]
    (fn [{:keys [lo hi marks on-press label]}]
      (let [ks (range (white-lo lo) (inc (white-hi hi)))
            whites (vec (filter t/white? ks))
            wi (zipmap whites (range))
            w 24 h 96 bw 15 bh 60
            press (fn [m e]
                    (.preventDefault e)
                    (swap! down conj m)
                    (on-press m))
            release (fn [m] (swap! down disj m))
            handlers (fn [m] {:on-pointer-down #(press m %)
                              :on-pointer-up #(release m)
                              :on-pointer-leave #(release m)
                              :on-pointer-cancel #(release m)})
            cls (fn [m] (str "k " (some-> (get marks m) name) (when (@down m) " down")))]
        [:div.piano-wrap {:ref center!}
         [:svg.piano {:viewBox (str "0 0 " (* w (count whites)) " " (inc h))
                      :style {:min-width (* 22 (count whites)) :max-width (* 48 (count whites))}}
          (for [m whites :let [x (* w (wi m))]]
            ^{:key m}
            [:g (merge {:class (cls m)} (handlers m))
             [:rect.white {:x x :y 0 :width w :height h :rx 3}]
             (when (zero? (mod m 12))
               [:text {:x (+ x (/ w 2)) :y (- h 8) :text-anchor "middle"} (label m)])])
          (for [m ks :when (not (t/white? m)) :let [x (- (* w (inc (wi (dec m)))) (/ bw 2))]]
            ^{:key m}
            [:g (merge {:class (cls m)} (handlers m))
             [:rect.black {:x x :y 0 :width bw :height bh :rx 2}]])]]))))
