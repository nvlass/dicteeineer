(ns dicteeineer.score
  (:require [reagent.core :as r]
            [dicteeineer.theory :as t]
            [dicteeineer.rhythm :as rh]))

;; One SVG per system (line of bars), all bars the same width. Inside a bar, notes sit at their
;; onsets, spaced by duration; the answer and the correct notes share the same onset grid.
;; 5px per diatonic step, so staff lines are 10px apart. On the grand staff middle C and up go on
;; the treble staff.

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
(def stem-len 32)

(defn- staff-y [clef staff]
  (if (and (= clef :grand) (= staff :bass)) (+ pad-top 40 staff-gap) pad-top))

(defn- y-of [clef staff step]
  (+ (staff-y clef staff) (* sp (- (get-in staves [staff :top]) step))))

(defn- note-y [clef n] (y-of clef (staff-of clef n) (:s n)))

(defn- height [clef] (+ pad-top 40 (if (= clef :grand) (+ staff-gap 40) 0) pad-bottom))

(defn x0 [{:keys [keysig time-sig?]}]
  (+ 40 (* 9 (Math/abs (t/signature keysig))) 8 (if time-sig? 24 0)))

(defn system-width [props n-bars] (+ (x0 props) (* n-bars (:bar-w props)) 6))

(defn- ledger-steps [staff step]
  (let [top (get-in staves [staff :top]) bottom (- top 8)]
    (concat (when (>= step (+ top 2)) (range (+ top 2) (inc step) 2))
            (when (<= step (- bottom 2)) (range (- bottom 2) (dec step) -2)))))

(defn- ellipse-path [x y rx ry phi]
  (let [c (Math/cos (* phi (/ Math/PI 180))) s (Math/sin (* phi (/ Math/PI 180)))]
    (str "M" (- x (* rx c)) "," (- y (* rx s))
         " A" rx "," ry " " phi " 1,0 " (+ x (* rx c)) "," (+ y (* rx s))
         " A" rx "," ry " " phi " 1,0 " (- x (* rx c)) "," (- y (* rx s)) " Z ")))

(defn- head-path [x y v]
  (case v
    :whole (str (ellipse-path x y 7.2 5 0) (ellipse-path x y 3.4 2.6 -35))
    :half (str (ellipse-path x y 6.4 4.5 -22) (ellipse-path x y 5.4 2.1 -22))
    (ellipse-path x y 6.2 4.4 -22)))

(defn- stem-x [x up?] (if up? (+ x 5.6) (- x 5.6)))

(defn- up? [clef notes]
  (let [n (first notes)
        mid (get-in staves [(staff-of clef n) :middle])]
    (< (/ (reduce + (map :s notes)) (count notes)) mid)))

(defn- flag-path [sx tip up? k]
  (let [y (if up? (+ tip (* 7 k)) (- tip (* 7 k)))
        d (if up? 1 -1)]
    (str "M" sx "," y " c 0.5," (* d 7) " 10," (* d 9) " 8," (* d 20))))

;; ---------------------------------------------------------------- one bar of notes

(defn- stems
  "Per note index: {:up? :tip y} plus beam polygons, flags and triplet numbers, for one voice."
  [clef bar xs]
  (let [ys (mapv #(note-y clef (:n %)) bar)
        groups (rh/beam-groups bar)
        beamed (set (mapcat #(when (> (count %) 1) %) groups))
        single (into {}
                     (for [i (range (count bar)) :when (not (beamed i))
                           :let [u (up? clef [(:n (bar i))])]]
                       [i {:up? u :tip (if u (- (ys i) stem-len) (+ (ys i) stem-len))}]))
        beams (for [g groups :when (> (count g) 1)
                    :let [u (up? clef (map #(:n (bar %)) g))
                          sx (mapv #(stem-x (xs %) u) g)
                          gy (mapv ys g)
                          dx (- (peek sx) (first sx))
                          slope (if (pos? dx) (max -0.12 (min 0.12 (/ (- (peek gy) (first gy)) dx 2))) 0)
                          base (if u
                                 (apply min (map-indexed #(- %2 stem-len (* slope (- (sx %1) (first sx)))) gy))
                                 (apply max (map-indexed #(+ %2 stem-len (- (* slope (- (sx %1) (first sx))))) gy)))
                          at (fn [x] (+ base (* slope (- x (first sx)))))]]
                {:g g :up? u :sx sx :at at})
        beam-tips (into {} (for [{:keys [g up? sx at]} beams [k i] (map-indexed vector g)]
                             [i {:up? up? :tip (at (sx k))}]))]
    {:stems (merge single beam-tips) :beams beams :ys ys}))

(defn- beam-els [{:keys [g up? sx at]} bar]
  (let [th (if up? 4.5 -4.5)
        off (if up? 7 -7)
        poly (fn [x1 x2 dy]
               (str x1 "," (+ (at x1) dy) " " x2 "," (+ (at x2) dy) " "
                    x2 "," (+ (at x2) dy th) " " x1 "," (+ (at x1) dy th)))
        sixteenth? #(= :16th (:v (bar (g %))))
        n (count g)]
    (into [:g [:polygon.beam {:points (poly (first sx) (peek sx) 0)}]]
          (for [k (range n) :when (sixteenth? k)
                :let [prev? (and (pos? k) (sixteenth? (dec k)))
                      next? (and (< (inc k) n) (sixteenth? (inc k)))]
                :when (not prev?)
                :let [x1 (sx k)
                      x2 (cond next? (sx (inc k))
                               (zero? k) (+ x1 9)
                               :else (- x1 9))]]
            [:polygon.beam {:points (poly (min x1 x2) (max x1 x2) off)}]))))

(defn- voice
  "Hiccup for one bar of events at x positions `xs`; `cls` gives a note's class."
  [clef keysig bar xs cls shift]
  (let [{:keys [stems beams ys]} (stems clef bar xs)
        accs (t/accidentals keysig (map :n bar))]
    (concat
     (for [i (range (count bar))
           :let [{:keys [n v dot]} (bar i)
                 x (+ shift (xs i))
                 y (ys i)
                 staff (staff-of clef n)
                 {u :up? tip :tip} (stems i)
                 sx (+ shift (stem-x (xs i) u))
                 acc (accs i)
                 on-line? (even? (- (get-in staves [staff :top]) (:s n)))]]
       ^{:key (str "n" i)}
       [:g {:class (str "note " (cls i))}
        (for [st (ledger-steps staff (:s n))]
          ^{:key st} [:line.ledger {:x1 (- x 10) :x2 (+ x 10) :y1 (y-of clef staff st) :y2 (y-of clef staff st)}])
        (when acc [:text.acc {:x (- x 10) :y (+ y 5) :text-anchor "end"} (if (zero? acc) "♮" (t/acc-sym acc))])
        [:path.head {:d (head-path x y v) :fill-rule "evenodd"}]
        (when dot [:circle.dot {:cx (+ x 11) :cy (if on-line? (- y 5) y) :r 1.9}])
        (when (not= v :whole)
          [:line.stem {:x1 sx :x2 sx :y1 (if u (- y 1) (+ y 1)) :y2 tip}])
        (when (and (#{:eighth :16th} v) (not (some #(some #{i} (:g %)) beams)))
          (for [k (range (if (= v :16th) 2 1))]
            ^{:key k} [:path.flag {:d (flag-path sx tip u k)}]))])
     (for [[k b] (map-indexed vector beams)]
       ^{:key (str "b" k)}
       [:g {:class (str "beam-group " (cls (first (:g b))))
            :transform (when-not (zero? shift) (str "translate(" shift ",0)"))}
        (beam-els b bar)])
     (for [g (rh/triplet-groups bar)
           :let [mid (/ (+ (xs (first g)) (xs (peek g))) 2)
                 u (:up? (stems (first g)))
                 tips (map #(:tip (stems %)) g)
                 y (if u (- (apply min tips) 6) (+ (apply max tips) 14))]]
       ^{:key (str "t" (first g))}
       [:text.tuplet {:x (+ shift mid 3) :y y :text-anchor "middle"} "3"]))))

;; ---------------------------------------------------------------- spacing

(defn- spacing
  "Onset → x inside a bar of width `w`, from the onsets of all voices plus `extra` points."
  [bar-len w voices extra]
  (let [pts (vec (sort (into (set extra) (mapcat rh/onsets voices))))
        pts (if (empty? pts) [0] pts)
        gaps (map - (concat (rest pts) [bar-len]) pts)
        nat (map #(+ 16 (* 5 (Math/sqrt (max 1 %)))) gaps)
        scale (/ (- w 22) (reduce + nat))
        xs (zipmap pts (map #(+ 14 (* scale %)) (reductions + 0 nat)))]
    (fn [o] (or (xs o) (+ 14 (* (- w 22) (/ o bar-len)))))))

;; ---------------------------------------------------------------- staff

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
       (let [x (- (x0 props) 14)]
         [:g.tsig
          [:text {:x x :y (+ y0 19) :text-anchor "middle"} beats]
          [:text {:x x :y (+ y0 39) :text-anchor "middle"} 4]]))]))

(defn- event-point [e]
  (let [svg (.-currentTarget e)
        rect (.getBoundingClientRect svg)
        vb (.. svg -viewBox -baseVal)
        k (/ (.-width vb) (.-width rect))]
    [(* k (- (.-clientX e) (.-left rect))) (* k (- (.-clientY e) (.-top rect)))]))

(defn single
  "One whole note, for the note mode."
  [{:keys [clef note acc cls zoom]}]
  (let [props {:keysig t/default-key}
        w 110 h (height clef) x 74]
    [:svg.score {:viewBox (str "0 0 " w " " h) :width w :height h :style {:width (* (or zoom 1) w)}}
     (for [staff (staff-names clef)]
       ^{:key staff} [staff-el clef staff props w])
     (let [staff (staff-of clef note) y (note-y clef note)]
       [:g {:class (str "note " cls)}
        (for [st (ledger-steps staff (:s note))]
          ^{:key st} [:line.ledger {:x1 (- x 11) :x2 (+ x 11) :y1 (y-of clef staff st) :y2 (y-of clef staff st)}])
        (when acc [:text.acc {:x (- x 12) :y (+ y 5) :text-anchor "end"} (t/acc-sym acc)])
        [:path.head {:d (head-path x y :whole) :fill-rule "evenodd"}]])]))

;; ---------------------------------------------------------------- systems

(defn system
  "Bars [from, from + n). `answer` and `ghosts` are bars of events, `marks` per bar a class per note."
  [_]
  (let [hover (r/atom nil)]
    (fn [{:keys [zoom clef keysig beats bar-w answer ghosts marks given sel active pickable playing checked
                 span on-pick dur from n final?] :as props}]
      (let [w (system-width props n)
            h (height clef)
            bx #(+ (x0 props) (* bar-w (- % from)))
            len (rh/bar-ticks beats)
            [lo hi] span
            layouts (into {}
                          (for [b (range from (+ from n))
                                :let [a (get answer b [])
                                      extra (when (and (= b pickable) (< (rh/total a) len)) [(rh/total a)])]]
                            [b (spacing len bar-w (keep identity [a (get ghosts b)]) extra)]))
            note-xs (fn [b bar] (mapv #((layouts b) %) (rh/onsets bar)))
            pick (fn [e]
                   (when (and pickable (<= from pickable (+ from n -1)))
                     (let [[x y] (event-point e)
                           a (get answer pickable [])
                           pts (cond-> (vec (map-indexed vector (rh/onsets a)))
                                 (< (rh/total a) len) (conj [(count a) (rh/total a)]))
                           rel (- x (bx pickable))]
                       (when (and (seq pts) (<= -4 rel (+ bar-w 4)))
                         (let [[i o] (apply min-key #(Math/abs (- rel ((layouts pickable) (second %)))) pts)
                               staff (if (and (= clef :grand) (> y (+ pad-top 40 (/ staff-gap 2)))) :bass (first (staff-names clef)))
                               step (+ (get-in staves [staff :top]) (Math/round (/ (- (staff-y clef staff) y) sp)))]
                           {:i i :x (+ (bx pickable) ((layouts pickable) o)) :step (max lo (min hi step))})))))
            top-y 6
            bot-y (- h 6)]
        [:svg.score {:viewBox (str "0 0 " w " " h) :width w :height h :style {:width (* (or zoom 1) w)}
                     :class (when on-pick "interactive")
                     :on-pointer-move (when on-pick #(reset! hover (pick %)))
                     :on-pointer-leave #(reset! hover nil)
                     :on-click (when on-pick #(when-let [{:keys [i step]} (pick %)] (on-pick i step)))}
         (when (and active (<= from active (+ from n -1)))
           [:rect.active {:x (+ (bx active) 2) :y top-y :width (- bar-w 4) :height (- bot-y top-y) :rx 6}])
         (when-let [[b i] sel]
           (when (<= from b (+ from n -1))
             (let [a (get answer b [])
                   o (if (< i (count a)) (get (rh/onsets a) i) (rh/total a))]
               (when (< o len)
                 [:rect.cursor {:x (- (+ (bx b) ((layouts b) o)) 11) :y top-y :width 22 :height (- bot-y top-y) :rx 5}]))))
         (when-let [[src b] playing]
           (when (and (= src :melody) (not (contains? checked b)) (<= from b (+ from n -1)))
             [:rect.playing {:x (+ (bx b) 2) :y top-y :width (- bar-w 4) :height (- bot-y top-y) :rx 6}]))
         (for [staff (staff-names clef)]
           ^{:key staff} [staff-el clef staff props w])
         (for [b (range 1 (inc n)) staff (staff-names clef)
               :let [x (+ (x0 props) (* b bar-w)) y0 (staff-y clef staff)]]
           ^{:key (str b staff)}
           [:line {:class (if (and final? (= b n)) "bar final" "bar") :x1 x :x2 x :y1 y0 :y2 (+ y0 40)}])
         (for [b (range from (+ from n))
               :let [g (get ghosts b)]
               :when (seq g)]
           ^{:key (str "g" b)}
           [:g.correct {:transform (str "translate(" (bx b) ",0)")}
            (voice clef keysig g (note-xs b g)
                   (fn [i] (when (= playing [:melody b i]) "playing")) 4)])
         (for [b (range from (+ from n))
               :let [a (get answer b [])]
               :when (seq a)]
           ^{:key (str "a" b)}
           [:g {:transform (str "translate(" (bx b) ",0)")}
            (voice clef keysig a (note-xs b a)
                   (fn [i] (str (some-> (get-in marks [b i]) name)
                                (when (contains? given [b i]) " given")
                                (when (= sel [b i]) " sel")
                                (when (or (= playing [:answer b i])
                                          (and (= playing [:melody b i]) (contains? checked b) (not (get ghosts b))))
                                  " playing")))
                   0)])
         (when-let [{:keys [x step]} (and on-pick @hover)]
           (let [note {:s step :a 0}
                 y (note-y clef note)
                 v (:v dur :quarter)
                 u (up? clef [note])
                 sx (stem-x x u)]
             [:g.note.ghost
              (for [st (ledger-steps (staff-of clef note) step)]
                ^{:key st} [:line.ledger {:x1 (- x 10) :x2 (+ x 10) :y1 (y-of clef (staff-of clef note) st) :y2 (y-of clef (staff-of clef note) st)}])
              [:path.head {:d (head-path x y v) :fill-rule "evenodd"}]
              (when (not= v :whole)
                [:line.stem {:x1 sx :x2 sx :y1 y :y2 (if u (- y stem-len) (+ y stem-len))}])]))]))))

(defn score
  "Wraps the bars into systems of `per-line` bars."
  [{:keys [per-line n-bars] :as props}]
  [:div.systems
   (for [from (range 0 n-bars per-line)]
     ^{:key from}
     [system (assoc props :from from :n (min per-line (- n-bars from))
                    :time-sig? (and (:time-sig? props) (zero? from))
                    :final? (>= (+ from per-line) n-bars))])])

(defn duration-icon
  "A small note for the duration buttons."
  [{:keys [v dot trip]}]
  (let [x 12 y 30
        u true
        sx (stem-x x u)
        tip (- y 24)]
    [:svg.dur-icon {:viewBox "0 0 34 40" :width 34 :height 40}
     [:path.head {:d (head-path x y v) :fill-rule "evenodd"}]
     (when (not= v :whole) [:line.stem {:x1 sx :x2 sx :y1 (- y 1) :y2 tip}])
     (when (#{:eighth :16th} v)
       (for [k (range (if (= v :16th) 2 1))]
         ^{:key k} [:path.flag {:d (str "M" sx "," (+ tip (* 6 k)) " c 0.5,6 9,8 7,17")}]))
     (when dot [:circle.dot {:cx (+ x 11) :cy y :r 2}])
     (when trip [:text.tuplet {:x 24 :y 12 :text-anchor "middle"} "3"])]))
