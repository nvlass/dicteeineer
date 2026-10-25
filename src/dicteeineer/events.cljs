(ns dicteeineer.events
  (:require [re-frame.core :as rf]
            [dicteeineer.theory :as t]
            [dicteeineer.melody :as melody]
            [dicteeineer.rhythm :as rh]
            [dicteeineer.audio :as audio]
            [dicteeineer.storage :as storage]
            [dicteeineer.midi :as midi]
            [dicteeineer.i18n :as i18n]))

(def default-settings
  {:lang (i18n/browser-lang)
   :note-names (if (= :el (i18n/browser-lang)) :solfege :letters)
   :clef :treble
   :width :narrow
   :midi false
   :note-octaves 1
   :note-accidentals false
   :note-reference :a4
   :auto-next true
   :dict-key t/default-key
   :dict-beats 4
   :dict-bars 4
   :dict-chromatic false
   :dict-repeats :strict
   :dict-first true
   :dict-rhythm 1
   :dict-count-in true})

(def references {:c4 60 :a4 69})

(def beat-s 0.75)
(def scale-s 0.4)
(def strict-plays 2)
(def max-attempts 20000)
(def max-history 500)
(def dict-version 2)

(def initial-db
  {:view :note
   :settings default-settings
   :loaded? false
   :samples nil
   :width 760
   :midi {:state :off :inputs []}
   :token 0
   :playing nil
   :note {:phase :idle :ref-heard false :session {:n 0 :ok 0}}
   :dict nil
   :attempts []
   :history []})

;; ---------------------------------------------------------------- effects

(rf/reg-fx :audio/play
  (fn [x]
    (audio/stop-all!)
    (if (map? x) (audio/play-events! (:events x) 1 (:clicks x)) (audio/play-events! x 1))))
(rf/reg-fx :audio/tap (fn [m] (audio/play-events! [[m 0 0.9]] 1)))
(rf/reg-fx :audio/stop (fn [_] (audio/stop-all!)))
(rf/reg-fx :later
  (fn [xs] (doseq [{:keys [ms event]} xs] (js/setTimeout #(rf/dispatch event) ms))))
(rf/reg-fx :midi/connect (fn [on?] (if on? (midi/connect!) (midi/disconnect!))))
(rf/reg-fx :html-lang (fn [lang] (set! (.. js/document -documentElement -lang) (name lang))))

(def persisted [:settings :attempts :history :dict])

(def persist
  "Saves the persisted parts of the db whenever they change, once storage has loaded."
  (rf/->interceptor
   :id :persist
   :after (fn [ctx]
            (let [before (get-in ctx [:coeffects :db])
                  after (get-in ctx [:effects :db])]
              (when (and after (:loaded? before))
                (doseq [k persisted
                        :when (not= (k before) (k after))]
                  (storage/save! k (k after)))))
            ctx)))

(rf/reg-global-interceptor persist)

;; ---------------------------------------------------------------- lifecycle

(rf/reg-event-db :init (fn [_ _] (assoc initial-db :width (.-innerWidth js/window))))

(rf/reg-event-fx :storage/loaded
  (fn [{:keys [db]} [_ {:keys [settings attempts history dict]}]]
    (let [s (merge default-settings settings)]
      (cond-> {:db (assoc db :settings s :attempts (vec attempts) :history (vec history)
                          :dict (when (= dict-version (:version dict))
                                  (cond-> dict (= :intro (:phase dict)) (assoc :phase :write)))
                          :loaded? true)
               :html-lang (:lang s)}
        (:midi s) (assoc :midi/connect true)))))

(rf/reg-event-db :audio/loaded (fn [db [_ n]] (assoc db :samples n)))
(rf/reg-event-db :resize (fn [db [_ w]] (assoc db :width w)))

(rf/reg-event-fx :set-view
  (fn [{:keys [db]} [_ v]]
    {:db (-> db (assoc :view v :playing nil) (update :token inc))
     :audio/stop true}))

(rf/reg-event-fx :settings/set
  (fn [{:keys [db]} [_ k v]]
    (cond-> {:db (assoc-in db [:settings k] v)}
      (= k :midi) (assoc :midi/connect v)
      (= k :lang) (assoc :html-lang v))))

(rf/reg-event-db :midi/status (fn [db [_ st]] (assoc db :midi st)))

;; ---------------------------------------------------------------- playback

(defn- play
  "Plays `items` ({:m midi :t start :d length, seconds, :at highlight}), after a bar of clicks when
   `count-in` is a number of beats, then dispatches `then`."
  [db items & {:keys [count-in then]}]
  (let [tok (inc (:token db))
        off (if count-in (* count-in beat-s) 0)
        end (+ off (reduce max 0 (map #(+ (:t %) (:d %)) items)))]
    {:db (assoc db :token tok :playing nil)
     :audio/play {:events (for [{:keys [m t d]} items :when m] [m (+ off t) (* 0.95 d)])
                  :clicks (when count-in (for [k (range count-in)] [(* k beat-s) (zero? k)]))}
     :later (concat
             (for [{:keys [t at]} items :when at]
               {:ms (* 1000 (+ off t)) :event [:play/at tok at]})
             [{:ms (* 1000 end) :event [:play/done tok then]}])}))

(defn- line-items
  "Even notes, as for the scale."
  [notes dur]
  (map-indexed (fn [i n] {:m (t/midi n) :t (* i dur) :d dur}) notes))

(def tick-s (/ beat-s rh/per-beat))

(defn- bar-items
  "Items for bars `from`..`to` of `bars` (`src` :melody or :answer), timed from the start of `from`."
  [src bars from to beats]
  (let [bar-len (rh/bar-ticks beats)]
    (for [b (range from (inc to))
          :let [bar (get bars b)]
          [i on] (map-indexed vector (rh/onsets bar))
          :let [e (bar i)]]
      {:m (t/midi (:n e)) :t (* tick-s (+ on (* bar-len (- b from)))) :d (* tick-s (rh/ticks e)) :at [src b i]})))

(rf/reg-event-db :play/at
  (fn [db [_ tok at]] (if (= tok (:token db)) (assoc db :playing at) db)))

(rf/reg-event-fx :play/done
  (fn [{:keys [db]} [_ tok then]]
    (when (= tok (:token db))
      (cond-> {:db (assoc db :playing nil)}
        then (assoc :dispatch then)))))

(rf/reg-event-fx :stop
  (fn [{:keys [db]} _]
    {:db (-> db (update :token inc) (assoc :playing nil)) :audio/stop true}))

;; ---------------------------------------------------------------- single note

(defn note-pool [{:keys [clef note-octaves note-accidentals]}]
  (let [[lo hi] (t/note-range clef note-octaves)]
    (filter #(or note-accidentals (t/white? %)) (range lo (inc hi)))))

(defn- next-target [db]
  (let [pool (note-pool (:settings db))
        last-t (get-in db [:note :target])]
    (rand-nth (or (seq (remove #{last-t} pool)) pool))))

(defn- ask [db with-ref?]
  (let [target (next-target db)
        ref (references (get-in db [:settings :note-reference]))
        ref? (and ref with-ref?)
        tok (inc (:token db))]
    {:db (-> db
             (assoc :token tok :playing nil)
             (update :note assoc :phase :ask :target target :answer nil :t0 (js/Date.now))
             (cond-> ref? (assoc-in [:note :ref-heard] true)))
     :audio/play (if ref? [[ref 0 1.0] [target 1.6 1.2]] [[target 0 1.2]])}))

(rf/reg-event-fx :note/start
  (fn [{:keys [db]} _]
    (ask db (not (get-in db [:note :ref-heard])))))

(rf/reg-event-fx :note/next
  (fn [{:keys [db]} [_ tok]]
    (when (or (nil? tok) (= tok (:token db)))
      (ask db false))))

(rf/reg-event-fx :note/hear
  (fn [{:keys [db]} [_ which]]
    (when-let [m (case which
                   :reference (references (get-in db [:settings :note-reference]))
                   (get-in db [:note which]))]
      {:audio/play [[m 0 1.2]]})))

(rf/reg-event-fx :note/answer
  (fn [{:keys [db]} [_ m]]
    (let [{:keys [phase target t0]} (:note db)
          s (:settings db)]
      (when (= phase :ask)
        (let [ok (= m target)
              tok (inc (:token db))
              attempt {:target target :answer m :correct ok :ms (- (js/Date.now) t0) :t (js/Date.now)
                       :octaves (:note-octaves s) :acc (:note-accidentals s)}]
          (cond-> {:db (-> db
                           (assoc :token tok)
                           (update :note assoc :phase :answered :answer m)
                           (update-in [:note :session :n] inc)
                           (cond-> ok (update-in [:note :session :ok] inc))
                           (update :attempts #(vec (take-last max-attempts (conj % attempt)))))}
            (and ok (:auto-next s)) (assoc :later [{:ms 1300 :event [:note/next tok]}])))))))

(rf/reg-event-db :note/reset-session
  (fn [db _] (assoc db :note {:phase :idle :ref-heard false :session {:n 0 :ok 0}})))

;; ---------------------------------------------------------------- dictation

(defn bar-len [d] (rh/bar-ticks (:beats d)))

(defn cur-bar [d] (get-in d [:answer (:bar d)] []))

(defn first-sel
  "The first index of the current bar that can be selected: 1 when the first note is given."
  [{:keys [given? bar]}]
  (if (and given? (zero? bar)) 1 0))

(defn writable? [{:keys [phase checked bar]}] (and (= :write phase) (not (contains? checked bar))))

(defn editable? [d i] (and (writable? d) (<= (first-sel d) i) (< i (count (cur-bar d)))))

(defn can-play? [d] (or (= :free (:repeats d)) (< (:plays d) strict-plays)))

(defn results
  "Totals over the checked bars."
  [d]
  (apply merge-with + {:n 0 :ok 0 :enh 0 :pitch 0 :rhythm 0}
         (map #(dissoc % :marks) (vals (:graded d)))))

(rf/reg-event-fx :dict/new
  (fn [{:keys [db]} _]
    (let [s (:settings db)
          rng (t/step-range (:clef s) (:width s))
          d0 {:version dict-version :key (:dict-key s) :beats (:dict-beats s) :bars (:dict-bars s)
              :clef (:clef s) :range rng :chromatic (:dict-chromatic s) :repeats (:dict-repeats s)
              :rhythm (:dict-rhythm s)}
          mel (melody/generate d0)
          given? (:dict-first s)
          d (assoc d0 :melody mel
                   :answer (into [(if given? [(first (first mel))] [])] (repeat (dec (count mel)) []))
                   :given? given? :bar 0 :checked #{} :graded {} :plays 0 :phase :intro :t (js/Date.now)
                   :dur {:v :quarter :dot false :trip false})
          d (assoc d :sel (count (get-in d [:answer 0])))]
      (play (assoc db :dict d) (line-items (melody/scale-notes (:key d) rng) scale-s) :then [:dict/intro-done]))))

(rf/reg-event-fx :dict/intro-done
  (fn [{:keys [db]} _]
    (when (= :intro (get-in db [:dict :phase]))
      {:db (-> db (assoc-in [:dict :phase] :write) (update :token inc) (assoc :playing nil))
       :audio/stop true
       :later [{:ms 700 :event [:dict/play 0]}]})))

(defn- count-in [db] (when (get-in db [:settings :dict-count-in]) (get-in db [:dict :beats])))

(rf/reg-event-fx :dict/play
  (fn [{:keys [db]} [_ bar]]
    (let [{:keys [melody beats] :as d} (:dict db)]
      (when (and d (= :write (:phase d)) (can-play? d) (or (nil? bar) (= bar (:bar d))))
        (play (update-in db [:dict :plays] inc)
              (bar-items :melody melody (max 0 (dec (:bar d))) (:bar d) beats)
              :count-in (count-in db))))))

(rf/reg-event-fx :dict/hear
  (fn [{:keys [db]} [_ which]]
    (let [{:keys [melody answer beats bar bars] :as d} (:dict db)]
      (case which
        :scale (play db (line-items (melody/scale-notes (:key d) (:range d)) scale-s))
        :bar (play db (bar-items :melody melody bar bar beats) :count-in (count-in db))
        :melody (play db (bar-items :melody melody 0 (dec bars) beats) :count-in (count-in db))
        :answer (play db (bar-items :answer answer 0 (dec bars) beats) :count-in (count-in db))))))

(defn- remove-at [v i] (into (subvec v 0 i) (subvec v (inc i))))

(defn- fit
  "The bar with `ev` at index `i` (replacing, or appended at the end), or nil if it overflows."
  [d bar i ev]
  (let [bar' (if (< i (count bar)) (assoc bar i ev) (conj bar ev))]
    (when (<= (rh/total bar') (bar-len d)) bar')))

(defn- triplet-done?
  "Whether the run of triplet notes ending at `i` completes a group."
  [bar i]
  (let [run (reverse (take-while :trip (reverse (subvec bar 0 (inc i)))))]
    (and (seq run) (zero? (mod (reduce + (map rh/ticks run)) (* 2 (rh/values (:v (first run)))))))))

(defn- set-bar [db bar sel]
  (-> db
      (assoc-in [:dict :answer (get-in db [:dict :bar])] bar)
      (update :dict assoc :sel sel :flash nil)))

(defn- place
  "Enters `note` at the cursor with the input duration, then moves on."
  [db note]
  (let [{:keys [sel dur] :as d} (:dict db)
        bar (cur-bar d)]
    (when (and (writable? d) (<= (first-sel d) sel (count bar)))
      (if-let [bar' (fit d bar sel (assoc dur :n note))]
        (cond-> (set-bar db bar' (min (inc sel) (count bar')))
          (and (:trip dur) (triplet-done? bar' sel)) (assoc-in [:dict :dur :trip] false))
        (assoc-in db [:dict :flash] :no-room)))))

(defn- prev-note [{:keys [answer bar sel]}]
  (or (:n (get-in answer [bar (dec sel)]))
      (:n (last (get answer (dec bar))))))

(rf/reg-event-fx :dict/pick
  (fn [{:keys [db]} [_ i step]]
    (let [{:keys [key] :as d} (:dict db)
          bar (cur-bar d)
          note {:s step :a (t/implied-alt key (map :n bar) i step)}]
      (cond
        (editable? d i) {:db (set-bar db (assoc-in bar [i :n] note) (min (inc i) (count bar)))
                         :audio/tap (t/midi note)}
        (= i (count bar)) (when-let [db' (place (assoc-in db [:dict :sel] i) note)]
                            {:db db' :audio/tap (t/midi note)})))))

(rf/reg-event-fx :dict/letter
  (fn [{:keys [db]} [_ l]]
    (let [{:keys [key sel] [lo hi] :range :as d} (:dict db)
          ref (or (:s (prev-note d)) (quot (+ lo hi) 2))
          s (first (filter #(= l (mod % 7)) (range (- ref 3) (+ ref 4))))
          s (cond (< s lo) (+ s 7) (> s hi) (- s 7) :else s)
          note {:s s :a (t/implied-alt key (map :n (cur-bar d)) sel s)}]
      (when-let [db' (place db note)]
        {:db db' :audio/tap (t/midi note)}))))

(rf/reg-event-fx :dict/midi
  (fn [{:keys [db]} [_ m]]
    (when-let [d (:dict db)]
      (when-let [db' (place db (t/spell (:key d) m))]
        {:db db'}))))

(defn- reshape
  "Applies `f` to the duration of the selected note, if it still fits."
  [db f]
  (let [{:keys [sel] :as d} (:dict db)
        bar (cur-bar d)]
    (if (editable? d sel)
      (if-let [bar' (fit d bar sel (f (bar sel)))]
        (set-bar db bar' sel)
        (assoc-in db [:dict :flash] :no-room))
      db)))

(rf/reg-event-db :dict/duration
  (fn [db [_ v]]
    (let [dot? (and (get-in db [:dict :dur :dot]) (rh/can-dot? v))]
      (-> db
          (update-in [:dict :dur] assoc :v v :dot dot?)
          (reshape #(assoc % :v v :dot (and (:dot %) (rh/can-dot? v))))))))

(rf/reg-event-db :dict/dot
  (fn [db _]
    (let [{:keys [v dot]} (get-in db [:dict :dur])]
      (-> db
          (cond-> (rh/can-dot? v) (update-in [:dict :dur] assoc :dot (not dot) :trip false))
          (reshape #(if (rh/can-dot? (:v %)) (assoc % :dot (not (:dot %)) :trip false) %))))))

(rf/reg-event-db :dict/triplet
  (fn [db _]
    (update-in db [:dict :dur] #(assoc % :trip (not (:trip %)) :dot false))))

(defn edit-target
  "The selected note, or the note just entered when the cursor is past it."
  [{:keys [sel] :as d}]
  (cond (editable? d sel) sel
        (editable? d (dec sel)) (dec sel)))

(rf/reg-event-fx :dict/edit
  (fn [{:keys [db]} [_ op]]
    (let [{:keys [key sel] :as d} (:dict db)
          bar (cur-bar d)
          i (edit-target d)
          note (when i (:n (bar i)))
          moved (fn [ds] (let [s (+ (:s note) ds)] {:s s :a (t/implied-alt key (map :n bar) i s)}))]
      (when (writable? d)
        (case op
          :left (when (> sel (first-sel d)) {:db (assoc-in db [:dict :sel] (dec sel))})
          :right (when (< sel (count bar)) {:db (assoc-in db [:dict :sel] (inc sel))})
          :delete (cond
                    (editable? d sel) {:db (set-bar db (remove-at bar sel) (min sel (dec (count bar))))}
                    (editable? d (dec sel)) {:db (set-bar db (remove-at bar (dec sel)) (dec sel))})
          (when note
            (let [note' (case op
                          :up (moved 1)
                          :down (moved -1)
                          :sharp (update note :a #(min 2 (inc %)))
                          :flat (update note :a #(max -2 (dec %)))
                          :natural (assoc note :a 0)
                          :respell (t/respell note))]
              {:db (set-bar db (assoc-in bar [i :n] note') sel)
               :audio/tap (t/midi note')})))))))

(rf/reg-event-fx :dict/check
  (fn [{:keys [db]} _]
    (let [{:keys [bar melody given?] :as d} (:dict db)]
      (when (and d (writable? d))
        (let [res (rh/grade-bar (melody bar) (cur-bar d) (if (and given? (zero? bar)) #{0} #{}))
              d (-> d
                    (update :checked conj bar)
                    (assoc-in [:graded bar] res)
                    (assoc :flash nil))
              last? (= bar (dec (:bars d)))
              d (cond-> d last? (assoc :phase :done))
              entry (when last?
                      (merge (select-keys d [:key :beats :bars :clef :chromatic :repeats :rhythm :t]) (results d)))]
          {:db (cond-> (assoc db :dict d)
                 entry (update :history #(vec (take-last max-history (conj % entry)))))})))))

(rf/reg-event-fx :dict/next
  (fn [{:keys [db]} _]
    (let [d (:dict db)]
      (when (and d (= :write (:phase d)) (contains? (:checked d) (:bar d)))
        {:db (update db :dict assoc :bar (inc (:bar d)) :plays 0 :sel 0)
         :dispatch [:dict/play]}))))

(rf/reg-event-fx :dict/quit
  (fn [{:keys [db]} _]
    {:db (-> db (assoc :dict nil :playing nil) (update :token inc)) :audio/stop true}))

;; ---------------------------------------------------------------- input

(rf/reg-event-fx :press
  (fn [{:keys [db]} [_ m]]
    (case (:view db)
      :note {:audio/tap m :dispatch [:note/answer m]}
      :dictation {:audio/tap m :dispatch [:dict/midi m]}
      {:audio/tap m})))

(rf/reg-event-fx :midi/note-on (fn [_ [_ m]] {:dispatch [:press m]}))
(rf/reg-event-db :midi/note-off (fn [db _] db))

(def duration-keys {"1" :16th "2" :eighth "3" :quarter "4" :half "5" :whole})

(def letter-keys {"c" 0 "d" 1 "e" 2 "f" 3 "g" 4 "a" 5 "b" 6})

(rf/reg-event-fx :key
  (fn [{:keys [db]} [_ k]]
    (let [lk (.toLowerCase k)]
      (case (:view db)
        :note (let [phase (get-in db [:note :phase])]
                (case k
                  "Enter" {:dispatch (if (= :ask phase) [:note/hear :target] [:note/start])}
                  " " (when (not= :idle phase) {:dispatch [:note/hear :target]})
                  ("r" "R") {:dispatch [:note/hear :reference]}
                  nil))
        :dictation (let [d (:dict db)
                         phase (:phase d)]
                     (cond
                       (not= :write phase) (when (= k "Enter") {:dispatch [:dict/new]})
                       (= k "Enter") {:dispatch (if (contains? (:checked d) (:bar d)) [:dict/next] [:dict/check])}
                       (= k " ") {:dispatch [:dict/play]}
                       (letter-keys lk) {:dispatch [:dict/letter (letter-keys lk)]}
                       (duration-keys k) (when (some #{(duration-keys k)} (rh/level-values (:rhythm d)))
                                           {:dispatch [:dict/duration (duration-keys k)]})
                       (= k ".") (when (>= (:rhythm d) 2) {:dispatch [:dict/dot]})
                       (= lk "t") (when (>= (:rhythm d) 6) {:dispatch [:dict/triplet]})
                       :else (when-let [op ({"ArrowUp" :up "ArrowDown" :down "ArrowLeft" :left "ArrowRight" :right
                                             "#" :sharp "-" :flat "n" :natural "N" :natural "Backspace" :delete
                                             "Delete" :delete "x" :respell} k)]
                               {:dispatch [:dict/edit op]})))
        nil))))

;; ---------------------------------------------------------------- stats

(rf/reg-event-db :stats/clear
  (fn [db [_ which]] (assoc db which [])))
