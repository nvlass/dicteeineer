(ns dicteeineer.views
  (:require [re-frame.core :as rf]
            [reagent.core :as r]
            [dicteeineer.theory :as t]
            [dicteeineer.events :as events]
            [dicteeineer.rhythm :as rh]
            [dicteeineer.score :as score]
            [dicteeineer.piano :refer [piano]]
            [dicteeineer.i18n :refer [tr]]))

(defn <sub [q] @(rf/subscribe q))
(defn >evt [e] (rf/dispatch e))

(defn seg [options value on-change]
  [:div.seg
   (for [[v label & [disabled]] options]
     ^{:key (str v)}
     [:button {:class (when (= v value) "on") :disabled disabled :on-click #(on-change v)} label])])

(defn toggle [label value on-change & [hint]]
  [:label.toggle
   [:input {:type "checkbox" :checked (boolean value) :on-change #(on-change (not value))}]
   [:span label (when hint [:small hint])]])

(defn field [label & body] (into [:div.field [:span label]] body))

(defn- setter [k] #(>evt [:settings/set k %]))

(defn- pct [x] (if x (str (Math/round (* 100 x)) "%") "–"))

(defn- c-label [m] (tr :fmt/note (t/spell t/default-key m)))

(defn- available-width []
  (- (min (<sub [:width]) 760) 66))

(defn- zoom [] (if (> (<sub [:width]) 600) 1.3 1))

(defn- per-line
  "The most bars per line that fit (allowing a slight shrink), dividing the bars evenly."
  [props bars]
  (or (first (filter #(and (zero? (mod bars %))
                           (<= (* 0.8 (:zoom props) (score/system-width (assoc props :time-sig? true) %)) (available-width)))
                     [4 3 2]))
      1))

;; ---------------------------------------------------------------- shared settings

(defn clef-fields []
  (let [s (<sub [:settings])]
    [:div.row2
     [field (tr :ui/clef)
      [seg [[:treble (tr :ui/treble)] [:bass (tr :ui/bass)] [:grand (tr :ui/grand)]] (:clef s) (setter :clef)]]
     [field (tr :ui/range)
      [seg (for [w [:narrow :wide]]
             [w (let [[lo hi] (t/step-range (:clef s) w)]
                  [:<> (tr (keyword "ui" (name w))) [:small.range (tr :fmt/note {:s lo :a 0}) "–" (tr :fmt/note {:s hi :a 0})]])])
       (:width s) (setter :width)]]]))

;; ---------------------------------------------------------------- single note

(defn note-options []
  (let [s (<sub [:settings])]
    [:details.card.options
     [:summary (tr :ui/options)
      [:span.hint (tr :fmt/note-summary s)]]
     [:div.row2
      [field (tr :ui/octaves)
       [seg [[1 (tr :ui/one-octave)] [3 (tr :ui/three-octaves)]] (:note-octaves s) (setter :note-octaves)]]
      [field (tr :ui/reference)
       [seg [[:none (tr :ui/none)] [:c4 (c-label 60)] [:a4 (c-label 69)]] (:note-reference s) (setter :note-reference)]]]
     (when (= 1 (:note-octaves s))
       [field (tr :ui/clef)
        [seg [[:treble (tr :ui/treble)] [:bass (tr :ui/bass)]] (if (= :bass (:clef s)) :bass :treble) (setter :clef)]])
     [toggle (tr :ui/accidentals) (:note-accidentals s) (setter :note-accidentals) (tr :ui/accidentals-hint)]
     [toggle (tr :ui/auto-next) (:auto-next s) (setter :auto-next)]
     [:p.hint (tr :ui/reference-hint)]]))

(defn note-view []
  (let [s (<sub [:settings])
        {:keys [phase target answer session]} (<sub [:note])
        [lo hi] (t/note-range (:clef s) (:note-octaves s))
        ok? (= target answer)
        clef (t/single-clef (:clef s) (:note-octaves s))
        ref (events/references (:note-reference s))]
    [:div
     [note-options]
     [:div.card.trainer
      [:div.status
       (case phase
         :idle [:div.prompt (tr :ui/note-intro)]
         :ask [:div.prompt (tr :ui/which-note)]
         :answered (if ok?
                     [:div.banner.good (tr :ui/correct) " " [:b (c-label target)]]
                     [:div.banner.bad (tr :fmt/wrong-note (c-label target) (c-label answer))]))
       (when (pos? (:n session))
         [:span.tally (:ok session) " / " (:n session)])]
      (when (= phase :answered)
        [:div.single
         [score/single {:clef clef :zoom 1.6 :cls "ok" :note (t/spell t/default-key target)
                        :acc (when-not (t/white? target) 1)}]])
      [:div.actions.center
       (if (= phase :idle)
         [:button.primary.big {:on-click #(>evt [:note/start])} (tr :ui/start)]
         [:<>
          [:button {:on-click #(>evt [:note/hear :target]) :title "Space"} (tr :ui/replay)]
          (when ref
            [:button {:on-click #(>evt [:note/hear :reference]) :title "R"} (tr :ui/reference-btn (c-label ref))])
          (when (and (= phase :answered) (not ok?))
            [:button {:on-click #(>evt [:note/hear :answer])} (tr :ui/hear-yours)])
          (when (= phase :answered)
            [:button.primary {:on-click #(>evt [:note/next]) :title "Enter"} (tr :ui/next)])])]]
     [piano {:lo lo :hi hi :label c-label
             :on-press #(>evt [:press %])
             :marks (when (= phase :answered)
                      (cond-> {target :target} (not ok?) (assoc answer :wrong)))}]
     [:p.hint.keys (tr :ui/note-keys)]]))

;; ---------------------------------------------------------------- dictation

(defn key-picker []
  (let [{:keys [tonic mode] :as k} (:dict-key (<sub [:settings]))]
    [:div.key-row
     [:select {:value (str (:l tonic) "/" (:a tonic))
               :on-change #(let [[l a] (map js/parseInt (.split (.. % -target -value) "/"))]
                             (>evt [:settings/set :dict-key (assoc k :tonic {:l l :a a})]))}
      (for [{:keys [tonic]} (t/keys-of mode)]
        ^{:key (str (:l tonic) (:a tonic))}
        [:option {:value (str (:l tonic) "/" (:a tonic))} (tr :fmt/tonic tonic)])]
     [seg [[:major (tr :ui/major)] [:minor (tr :ui/minor)]] mode
      (fn [m] (let [k' {:tonic tonic :mode m}]
                (>evt [:settings/set :dict-key (if (some #{k'} (t/keys-of m)) k' (assoc t/default-key :mode m))])))]
     [:span.hint (tr :fmt/signature (t/signature k))]]))

(defn dict-setup []
  (let [s (<sub [:settings])]
    [:div.card.setup
     [field (tr :ui/key) [key-picker]]
     [:div.row2
      [field (tr :ui/meter) [seg [[4 "4/4"] [3 "3/4"]] (:dict-beats s) (setter :dict-beats)]]
      [field (tr :ui/bars) [seg [[4 "4"] [8 "8"] [12 "12"] [:inf "∞" true]] (:dict-bars s) (setter :dict-bars)]]]
     [clef-fields]
     [field (tr :ui/repeats)
      [seg [[:strict (tr :ui/strict)] [:free (tr :ui/free)]] (:dict-repeats s) (setter :dict-repeats)]]
     [field (tr :ui/rhythm)
      [seg (for [l (range 1 (inc rh/max-level))] [l (str l)]) (:dict-rhythm s) (setter :dict-rhythm)]
      [:small.level-hint (tr (keyword "rhythm" (str "l" (:dict-rhythm s))))]]
     [toggle (tr :ui/chromatic) (:dict-chromatic s) (setter :dict-chromatic) (tr :ui/chromatic-hint)]
     [toggle (tr :ui/count-in) (:dict-count-in s) (setter :dict-count-in)]
     [toggle (tr :ui/first-given) (:dict-first s) (setter :dict-first)]
     [:div.actions.center
      [:button.primary.big {:on-click #(>evt [:dict/new]) :title "Enter"} (tr :ui/start-dictation)]]]))

(defn duration-bar []
  (let [{:keys [dur rhythm]} (<sub [:dict])
        vs (rh/level-values rhythm)]
    (when (> (count vs) 1)
      [:div.durations
       (for [[k v] (map vector (range) [:16th :eighth :quarter :half :whole])
             :when (some #{v} vs)]
         ^{:key v}
         [:button.dur {:class (when (= v (:v dur)) "on") :title (tr (keyword "dur" (name v)))
                       :on-click #(>evt [:dict/duration v])}
          [score/duration-icon {:v v}] [:small (inc k)]])
       [:button.dur {:class (when (:dot dur) "on") :disabled (not (rh/can-dot? (:v dur))) :title (tr :dur/dot)
                     :on-click #(>evt [:dict/dot])}
        [:span.glyph "•"] [:small "."]]
       (when (>= rhythm 6)
         [:button.dur {:class (when (:trip dur) "on") :title (tr :dur/triplet) :on-click #(>evt [:dict/triplet])}
          [:span.glyph "3"] [:small "T"]])])))

(defn edit-tools []
  (let [d (<sub [:dict])
        note (some->> (events/edit-target d) (get (events/cur-bar d)))
        can? (some? note)]
    [:div.tools
     [:button {:title (tr :ui/tool-up) :disabled (not note) :on-click #(>evt [:dict/edit :up])} "↑"]
     [:button {:title (tr :ui/tool-down) :disabled (not note) :on-click #(>evt [:dict/edit :down])} "↓"]
     [:button {:title (tr :ui/tool-sharp) :disabled (not note) :on-click #(>evt [:dict/edit :sharp])} "♯"]
     [:button {:title (tr :ui/tool-flat) :disabled (not note) :on-click #(>evt [:dict/edit :flat])} "♭"]
     [:button {:title (tr :ui/tool-natural) :disabled (not note) :on-click #(>evt [:dict/edit :natural])} "♮"]
     [:button {:title (tr :ui/tool-respell) :disabled (not note) :on-click #(>evt [:dict/edit :respell])} "⇄"]
     [:button {:title (tr :ui/tool-delete) :disabled (not can?) :on-click #(>evt [:dict/edit :delete])} "⌫"]]))

(def beat-width {1 30 2 30 3 40 4 40 5 54 6 54})

(defn dict-score []
  (let [d (<sub [:dict])
        {:keys [clef key beats melody answer checked graded given? sel phase bar rhythm dur] span :range} d
        done? (= phase :done)
        writing? (and (= phase :write) (not (contains? checked bar)))
        marks (into {} (for [[b res] graded] [b (:marks res)]))
        ghosts (into {} (for [[b res] graded
                              :when (or (< (:ok res) (:n res)) (some #(not= :ok %) (:marks res)))]
                          [b (melody b)]))
        narrow? (<= (<sub [:width]) 600)
        props {:clef clef :keysig key :beats beats :time-sig? true :zoom (zoom)
               :bar-w (+ 18 (* beats (cond-> (beat-width rhythm) narrow? (* 0.85))))}]
    [:div.score-wrap
     [score/score (merge props
                         {:per-line (per-line props (:bars d)) :n-bars (count melody) :span span
                          :answer answer :ghosts ghosts :marks marks :checked checked
                          :given (if given? #{[0 0]} #{})
                          :sel (when writing? [bar sel])
                          :active (when-not done? bar)
                          :pickable (when writing? bar)
                          :dur dur
                          :playing (<sub [:playing])
                          :on-pick (when writing? #(>evt [:dict/pick %1 %2]))})]]))

(defn dict-status []
  (let [d (<sub [:dict])
        {:keys [bar bars phase checked repeats plays graded flash]} d
        checked? (contains? checked bar)]
    (case phase
      :intro [:div.banner (tr :ui/listen-scale)]
      :done (let [{:keys [n ok enh pitch rhythm]} (events/results d)
                  n (max 1 n)]
              [:div.banner {:class (if (>= (/ (+ ok enh) n) 0.8) "good" "bad")}
               (tr :fmt/result ok enh n)
               (when (> (:rhythm d) 1)
                 [:div.sub (tr :fmt/pitch-rhythm (pct (/ pitch n)) (pct (/ rhythm n)))])])
      [:div.dict-bar
       [:span.progress (tr :fmt/bar-of (inc bar) bars)
        (when (pos? bar) [:small (tr :fmt/hearing (inc bar))])]
       (when checked?
         (let [{:keys [marks n ok enh]} (graded bar)
               bad (max (- n ok enh) (count (remove #{:ok :enh} marks)))]
           [:span {:class (if (zero? bad) "ok-text" "err-text")}
            (if (zero? bad) (tr :ui/bar-correct) (tr :fmt/bar-wrong bad))]))
       (when (and (= repeats :strict) (not checked?))
         [:span.plays (tr :fmt/plays-left (max 0 (- events/strict-plays plays)))])
       (when (= flash :no-room) [:span.err-text (tr :ui/no-room)])])))

(defn dict-actions []
  (let [d (<sub [:dict])
        {:keys [phase checked bar repeats]} d
        checked? (contains? checked bar)
        playing? (some? (<sub [:playing]))]
    [:div.actions
     (case phase
       :intro [:button {:on-click #(>evt [:dict/intro-done])} (tr :ui/skip)]
       :done [:<>
              [:button {:on-click #(>evt [:dict/hear :melody])} (tr :ui/hear-melody)]
              [:button {:on-click #(>evt [:dict/hear :answer])} (tr :ui/hear-answer)]
              [:button.primary {:on-click #(>evt [:dict/new])} (tr :ui/new-dictation)]]
       [:<>
        (if checked?
          [:button {:on-click #(>evt [:dict/hear :bar])} (tr :ui/hear-bar)]
          [:button {:disabled (not (events/can-play? d)) :on-click #(>evt [:dict/play]) :title "Space"} (tr :ui/play-step)])
        (when (= repeats :free)
          [:button {:on-click #(>evt [:dict/hear :scale])} (tr :ui/hear-scale)])
        (if checked?
          [:button.primary {:on-click #(>evt [:dict/next]) :title "Enter"} (tr :ui/next-bar)]
          [:button.primary {:on-click #(>evt [:dict/check]) :title "Enter"} (tr :ui/check)])])
     (when playing? [:button.ghost {:on-click #(>evt [:stop])} "■"])]))

(defn dictation-view []
  (let [d (<sub [:dict])]
    (if-not d
      [dict-setup]
      (let [[lo hi] (:range d)]
        [:div
         [:div.card.trainer
          [:div.dict-head
           [:span.hint (tr :fmt/dict-summary d)]
           [:button.link {:on-click #(>evt [:dict/quit])} (tr (if (= :done (:phase d)) :ui/settings-back :ui/quit))]]
          [dict-status]
          (when (events/writable? d) [:div.input-bar [duration-bar] [edit-tools]])
          [dict-score]
          [dict-actions]]
         [piano {:lo (t/midi {:s lo :a 0}) :hi (t/midi {:s hi :a 0}) :label c-label
                 :on-press #(>evt [:press %])}]
         [:p.hint.keys (tr :ui/dict-keys)]]))))

;; ---------------------------------------------------------------- stats

(defn meter-row [label {:keys [n acc]}]
  [:div.meter-row
   [:span label]
   [:div.meter [:span {:style {:width (pct acc)}}]]
   [:b (pct acc) [:small " · " n]]])

(defn clear-button [which]
  (let [armed (r/atom false)]
    (fn [which]
      (if @armed
        [:span.confirm (tr :ui/sure)
         [:button.bad {:on-click #(do (reset! armed false) (>evt [:stats/clear which]))} (tr :ui/clear)]
         [:button {:on-click #(reset! armed false)} (tr :ui/cancel)]]
        [:button.link {:on-click #(reset! armed true)} (tr :ui/clear-stats)]))))

(defn stats-view []
  (let [{:keys [total recent by-pc by-octave confusions]} (<sub [:note-stats])
        hist (<sub [:history])]
    [:div
     [:div.card
      [:h3 (tr :nav/note)]
      (if (zero? (:n total))
        [:p.hint (tr :ui/no-attempts)]
        [:<>
         [:div.big-stats
          [:div [:b (pct (:acc total))] [:small (tr :fmt/attempts (:n total))]]
          [:div [:b (pct (:acc recent))] [:small (tr :ui/last-20)]]]
         [:div.stat-cols
          [:div
           [:div.group-label (tr :ui/by-note)]
           (for [{:keys [key] :as row} by-pc]
             ^{:key key} [meter-row (tr :fmt/pc key) row])]
          [:div
           (when (> (count by-octave) 1)
             [:<>
              [:div.group-label (tr :ui/by-octave)]
              (for [{:keys [key] :as row} by-octave]
                ^{:key key} [meter-row (str key) row])])
           (when (seq confusions)
             [:<>
              [:div.group-label (tr :ui/confusions)]
              [:ul.confusions
               (for [{:keys [target answer n]} confusions]
                 ^{:key (str target answer)}
                 [:li (c-label target) " → " (c-label answer) [:small " ×" n]])]])]]
         [clear-button :attempts]])]
     [:div.card
      [:h3 (tr :nav/dictation)]
      (if (empty? hist)
        [:p.hint (tr :ui/no-dictations)]
        [:<>
         [:ul.history
          (for [{:keys [t n ok enh] :as h} (take 30 (reverse hist))]
            ^{:key t}
            [:li
             [:span.date (.toLocaleDateString (js/Date. t))]
             [:span (tr :fmt/dict-summary h)]
             [:b (pct (/ (+ ok enh) (max 1 n)))]])]
         [clear-button :history]])]]))

;; ---------------------------------------------------------------- settings

(defn settings-view []
  (let [s (<sub [:settings])
        {:keys [state inputs]} (<sub [:midi])]
    [:div.card
     [field (tr :ui/language) [seg [[:en "English"] [:el "Ελληνικά"]] (:lang s) (setter :lang)]]
     [field (tr :ui/note-names)
      [seg [[:letters "C D E"] [:solfege (tr :ui/solfege-sample)]] (:note-names s) (setter :note-names)]]
     [clef-fields]
     [toggle (tr :ui/midi) (:midi s) (setter :midi)
      (case state
        :ready (if (seq inputs) (apply str inputs) (tr :ui/midi-none))
        :unsupported (tr :ui/midi-unsupported)
        :denied (tr :ui/midi-denied)
        (tr :ui/midi-hint))]]))

;; ---------------------------------------------------------------- app

(defn app []
  (let [view (<sub [:view])
        s (<sub [:settings])]
    [:div.app
     [:header
      [:h1 "Dicteeineer"]
      [:div.header-end
       (when-not (<sub [:samples]) [:span.badge (tr :ui/loading-sounds)])
       [:div.lang
        [seg [[:en "EN"] [:el "ΕΛ"]] (:lang s) (setter :lang)]]]]
     [:nav
      (for [v [:note :dictation :stats :settings]]
        ^{:key v}
        [:button {:class (when (= v view) "on") :on-click #(>evt [:set-view v])} (tr (keyword "nav" (name v)))])]
     (when (zero? (<sub [:samples])) [:div.banner.warn (tr :ui/samples-failed)])
     (case view
       :dictation [dictation-view]
       :stats [stats-view]
       :settings [settings-view]
       [note-view])]))
