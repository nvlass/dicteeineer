(ns dicteeiner.score
  (:require [reagent.core :as r]
            [dicteeiner.theory :as t]))

;; One SVG per system (line of bars). 5px per diatonic step, so staff lines are 10px apart.
;; On the grand staff notes from middle C up go on the treble staff.

(def sp 5)

(def staves
  {:treble {:top 38 :middle 34}
   :bass {:top 26 :middle 22}})

(def sharp-steps {:treble [38 35 39 36 33 37 34] :bass [24 21 25 22 19 23 20]})
(def flat-steps {:treble [34 37 33 36 32 35 31] :bass [20 23 19 22 18 21 17]})

(defn staff-names [clef] (if (= clef :grand) [:treble :bass] [clef]))

(defn staff-of [clef {:keys [s]}]
  (if (= clef :grand) (if (>= s 28) :treble :bass) clef))

(def pad-top 40)
(def staff-gap 56)
(def pad-bottom 40)

(defn- staff-y [clef staff]
  (if (and (= clef :grand) (= staff :bass)) (+ pad-top 40 staff-gap) pad-top))

(defn- y-of [clef staff step]
  (+ (staff-y clef staff) (* sp (- (get-in staves [staff :top]) step))))

(defn- height [clef] (+ pad-top 40 (if (= clef :grand) (+ staff-gap 40) 0) pad-bottom))

(defn layout
  "Geometry for a system of `n-bars` bars of `beats` columns."
  [{:keys [keysig beats col-w time-sig?]} n-bars]
  (let [x0 (+ 40 (* 9 (Math/abs (t/signature keysig))) 8 (if time-sig? 24 0))
        bar-w (+ 10 (* beats col-w))]
    {:x0 x0 :bar-w bar-w :col-w col-w :beats beats
     :w (+ x0 (* n-bars bar-w) 6)}))

(defn system-width [props n-bars] (:w (layout props n-bars)))

(defn- cx [{:keys [x0 bar-w col-w beats]} i]
  (+ x0 (* bar-w (quot i beats)) 10 (* col-w (+ (mod i beats) 0.5))))

(defn- ledger-steps [staff step]
  (let [top (get-in staves [staff :top]) bottom (- top 8)]
    (concat (when (>= step (+ top 2)) (range (+ top 2) (inc step) 2))
            (when (<= step (- bottom 2)) (range (- bottom 2) (dec step) -2)))))

(defn- ellipse-path [x y rx ry phi]
  (let [c (Math/cos (* phi (/ Math/PI 180))) s (Math/sin (* phi (/ Math/PI 180)))]
    (str "M" (- x (* rx c)) "," (- y (* rx s))
         " A" rx "," ry " " phi " 1,0 " (+ x (* rx c)) "," (+ y (* rx s))
         " A" rx "," ry " " phi " 1,0 " (- x (* rx c)) "," (- y (* rx s)) " Z ")))

(defn- head-path [x y dur]
  (if (= dur :whole)
    (str (ellipse-path x y 7.2 5 0) (ellipse-path x y 3.4 2.6 -35))
    (ellipse-path x y 6.2 4.4 -22)))

(defn note-el [clef x note acc cls dur]
  (let [staff (staff-of clef note)
        y (y-of clef staff (:s note))
        up? (< (:s note) (get-in staves [staff :middle]))]
    [:g {:class (str "note " cls)}
     (for [st (ledger-steps staff (:s note))]
       ^{:key st} [:line.ledger {:x1 (- x 10) :x2 (+ x 10) :y1 (y-of clef staff st) :y2 (y-of clef staff st)}])
     (when acc [:text.acc {:x (- x 10) :y (+ y 5) :text-anchor "end"} (if (zero? acc) "♮" (t/acc-sym acc))])
     [:path.head {:d (head-path x y dur) :fill-rule "evenodd"}]
     (when-not (= dur :whole)
       (let [sx (if up? (+ x 5.5) (- x 5.5))]
         [:line.stem {:x1 sx :x2 sx :y1 (if up? (- y 1) (+ y 1)) :y2 (if up? (- y 32) (+ y 32))}]))]))

(defn- clef-glyph
  "Clef glyphs differ a lot between fonts, so measure the glyph and scale it into the given box."
  [_ _ _ _]
  (let [el (atom nil)
        bbox (r/atom nil)]
    (r/create-class
     {:component-did-mount
      (fn [] (when-let [n @el]
               (let [b (.getBBox n)]
                 (when (pos? (.-height b)) (reset! bbox [(.-x b) (.-y b) (.-height b)])))))
      :reagent-render
      (fn [glyph x y-top h]
        (let [[bx by bh] @bbox]
          [:text.clef {:ref #(reset! el %) :x 0 :y 0 :font-size 40
                       :visibility (if bh "visible" "hidden")
                       :transform (when bh (str "translate(" x "," y-top ") scale(" (/ h bh) ") translate(" (- bx) "," (- by) ")"))}
           glyph]))})))

(defn- staff-el [clef staff {:keys [keysig beats time-sig?] :as props} w]
  (let [y0 (staff-y clef staff)
        sig (t/signature keysig)
        steps (take (Math/abs sig) ((if (neg? sig) flat-steps sharp-steps) staff))]
    [:g.staff
     (for [k (range 5)]
       ^{:key k} [:line {:x1 2 :x2 (- w 4) :y1 (+ y0 (* 10 k)) :y2 (+ y0 (* 10 k))}])
     [:line {:x1 2 :x2 2 :y1 y0 :y2 (+ y0 40)}]
     (if (= staff :treble)
       [clef-glyph "𝄞" 6 (- y0 13) 68]
       [clef-glyph "𝄢" 7 (- y0 1) 32])
     (for [[k st] (map-indexed vector steps)]
       ^{:key k}
       [:text.acc {:x (+ 38 (* 9 k)) :y (+ (y-of clef staff st) 5)} (if (neg? sig) "♭" "♯")])
     (when time-sig?
       (let [x (- (:x0 (layout props 1)) 14)]
         [:g.tsig
          [:text {:x x :y (+ y0 19) :text-anchor "middle"} beats]
          [:text {:x x :y (+ y0 39) :text-anchor "middle"} 4]]))]))

(defn- event-point [e]
  (let [svg (.-currentTarget e)
        rect (.getBoundingClientRect svg)
        vb (.. svg -viewBox -baseVal)
        k (/ (.-width vb) (.-width rect))]
    [(* k (- (.-clientX e) (.-left rect))) (* k (- (.-clientY e) (.-top rect)))]))

(defn- pick-at [clef lay [lo hi] n x y]
  (let [{:keys [x0 bar-w col-w beats]} lay
        bar (Math/floor (/ (- x x0) bar-w))
        beat (Math/floor (/ (- x x0 (* bar bar-w) 10) col-w))
        i (+ (* bar beats) beat)]
    (when (and (<= 0 beat (dec beats)) (< -1 i n))
      (let [staff (if (and (= clef :grand) (> y (+ pad-top 40 (/ staff-gap 2)))) :bass (first (staff-names clef)))
            step (+ (get-in staves [staff :top]) (Math/round (/ (- (staff-y clef staff) y) sp)))]
        {:i i :step (max lo (min hi step))}))))

(defn system
  "One line of bars. Slots are indices into the whole melody; this system shows [from, from + n)."
  [_]
  (let [hover (r/atom nil)]
    (fn [{:keys [zoom clef keysig beats notes accs ghosts marks sel active playing span on-pick pickable? dur from n final?]
          :or {dur :quarter} :as props}]
      (let [n-bars (Math/ceil (/ n beats))
            lay (layout props n-bars)
            w (:w lay)
            h (height clef)
            top-y 6
            bot-y (- h 6)
            x-of #(cx lay (- % from))
            pick (fn [e] (let [[x y] (event-point e)
                               p (pick-at clef lay span n x y)]
                           (when (and p (pickable? (+ from (:i p)))) (update p :i + from))))]
        [:svg.score {:viewBox (str "0 0 " w " " h) :width w :height h :style {:width (* (or zoom 1) w)}
                     :class (when on-pick "interactive")
                     :on-pointer-move (when on-pick #(reset! hover (pick %)))
                     :on-pointer-leave #(reset! hover nil)
                     :on-click (when on-pick #(when-let [{:keys [i step]} (pick %)] (on-pick i step)))}
         (when-let [[a b] active]
           (let [a (max a from) b (min b (+ from n))]
             (when (< a b)
               [:rect.active {:x (- (x-of a) (/ (:col-w lay) 2) 4) :y top-y
                              :width (+ 8 (* (:col-w lay) (- b a))) :height (- bot-y top-y) :rx 6}])))
         (when (and sel (<= from sel (+ from n -1)))
           [:rect.cursor {:x (- (x-of sel) (/ (:col-w lay) 2) -2) :y top-y
                          :width (- (:col-w lay) 4) :height (- bot-y top-y) :rx 5}])
         (when (and playing (<= from playing (+ from n -1)))
           [:rect.playing {:x (- (x-of playing) (/ (:col-w lay) 2) -2) :y top-y
                           :width (- (:col-w lay) 4) :height (- bot-y top-y) :rx 5}])
         (for [staff (staff-names clef)]
           ^{:key staff} [staff-el clef staff props w])
         (for [b (range 1 (inc n-bars)) staff (staff-names clef)
               :let [x (+ (:x0 lay) (* b (:bar-w lay))) y0 (staff-y clef staff)]
               :when (not= dur :whole)]
           ^{:key (str b staff)}
           [:line {:class (if (and final? (= b n-bars)) "bar final" "bar") :x1 x :x2 x :y1 y0 :y2 (+ y0 40)}])
         (for [i (range from (+ from n))
               :let [note (get notes i)]
               :when note]
           ^{:key (str "n" i)}
           [note-el clef (x-of i) note (get accs i)
            (str (some-> (get marks i) name) (when (= i sel) " sel")) dur])
         (for [[i note] ghosts
               :when (<= from i (+ from n -1))]
           ^{:key (str "g" i)}
           [note-el clef (+ (x-of i) 3) note (when (not= (:a note) (t/sig-alt keysig (t/letter note))) (:a note))
            "correct" dur])
         (when-let [{:keys [i step]} (and on-pick @hover)]
           (when-not (= step (:s (get notes i)))
             [note-el clef (x-of i) {:s step :a 0} nil "ghost" dur]))]))))

(defn score
  "Wraps the melody into systems of `per-line` bars."
  [{:keys [beats per-line n] :as props}]
  (let [spl (* beats per-line)]
    [:div.systems
     (for [from (range 0 n spl)]
       ^{:key from}
       [system (assoc props :from from :n (min spl (- n from)) :time-sig? (and (:time-sig? props) (zero? from))
                      :final? (>= (+ from spl) n))])]))
