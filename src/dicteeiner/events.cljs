(ns dicteeiner.events
  (:require [re-frame.core :as rf]
            [dicteeiner.theory :as t]
            [dicteeiner.melody :as melody]
            [dicteeiner.audio :as audio]
            [dicteeiner.storage :as storage]
            [dicteeiner.midi :as midi]
            [dicteeiner.i18n :as i18n]))

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
   :dict-first true})

(def references {:c4 60 :a4 69})

(def beat-s 0.75)
(def scale-s 0.4)
(def strict-plays 2)
(def max-attempts 20000)
(def max-history 500)

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

(rf/reg-fx :audio/play (fn [events] (audio/stop-all!) (audio/play-events! events 1)))
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
                          :dict (cond-> dict (= :intro (:phase dict)) (assoc :phase :write))
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
  "Plays `notes` (MIDI) one after another, highlighting slot `first-slot` onwards when given."
  [db notes dur & [first-slot then]]
  (let [tok (inc (:token db))]
    {:db (assoc db :token tok :playing (when first-slot first-slot))
     :audio/play (keep-indexed (fn [i m] (when m [m (* i dur) (* 0.95 dur)])) notes)
     :later (concat
             (when first-slot
               (for [i (range 1 (count notes))]
                 {:ms (* 1000 i dur) :event [:play/at tok (+ first-slot i)]}))
             [{:ms (* 1000 (count notes) dur) :event [:play/done tok then]}])}))

(rf/reg-event-db :play/at
  (fn [db [_ tok slot]] (if (= tok (:token db)) (assoc db :playing slot) db)))

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

(defn n-slots [{:keys [beats bars]}] (* beats bars))

(defn bar-slots [{:keys [beats bar]}] [(* beats bar) (* beats (inc bar))])

(defn- in-bar? [d i] (let [[a b] (bar-slots d)] (and (<= a i) (< i b))))

(defn editable? [d i] (and (= :write (:phase d)) (in-bar? d i) (not (contains? (:given d) i))
                           (not (contains? (:checked d) (:bar d)))))

(defn can-play? [d] (or (= :free (:repeats d)) (< (:plays d) strict-plays)))

(defn grade [d i]
  (let [want (get-in d [:melody i]) got (get-in d [:answer i])]
    (cond (nil? got) :wrong
          (= want got) :ok
          (= (t/midi want) (t/midi got)) :enh
          :else :wrong)))

(defn results [d]
  (let [gs (map #(grade d %) (remove (:given d) (range (n-slots d))))]
    {:n (count gs) :ok (count (filter #{:ok} gs)) :enh (count (filter #{:enh} gs))}))

(defn- step-notes
  "The previous bar and the current one."
  [{:keys [beats bar melody]}]
  (let [from (* beats (max 0 (dec bar)))]
    [from (subvec melody from (* beats (inc bar)))]))

(defn- first-free [d]
  (let [d (assoc d :phase :write)
        [a b] (bar-slots d)]
    (or (first (filter #(and (nil? (get-in d [:answer %])) (editable? d %)) (range a b)))
        (first (filter #(editable? d %) (range a b)))
        a)))

(rf/reg-event-fx :dict/new
  (fn [{:keys [db]} _]
    (let [s (:settings db)
          rng (t/step-range (:clef s) (:width s))
          d0 {:key (:dict-key s) :beats (:dict-beats s) :bars (:dict-bars s) :clef (:clef s) :range rng
              :chromatic (:dict-chromatic s) :repeats (:dict-repeats s)}
          mel (melody/generate d0)
          given (if (:dict-first s) #{0} #{})
          d (assoc d0 :melody mel
                   :answer (vec (repeat (count mel) nil))
                   :given given :bar 0 :checked #{} :plays 0 :phase :intro :t (js/Date.now))
          d (cond-> d (given 0) (assoc-in [:answer 0] (first mel)))
          d (assoc d :sel (first-free d))
          scale (melody/scale-notes (:key d) rng)]
      (play (assoc db :dict d) (map t/midi scale) scale-s nil [:dict/intro-done]))))

(rf/reg-event-fx :dict/intro-done
  (fn [{:keys [db]} _]
    (when (= :intro (get-in db [:dict :phase]))
      {:db (-> db (assoc-in [:dict :phase] :write) (update :token inc) (assoc :playing nil))
       :audio/stop true
       :later [{:ms 700 :event [:dict/play 0]}]})))

(rf/reg-event-fx :dict/play
  (fn [{:keys [db]} [_ bar]]
    (let [d (:dict db)]
      (when (and d (= :write (:phase d)) (can-play? d) (or (nil? bar) (= bar (:bar d))))
        (let [[from notes] (step-notes d)]
          (play (update-in db [:dict :plays] inc) (map t/midi notes) beat-s from))))))

(rf/reg-event-fx :dict/hear
  (fn [{:keys [db]} [_ which]]
    (let [d (:dict db)]
      (case which
        :scale (play db (map t/midi (melody/scale-notes (:key d) (:range d))) scale-s)
        :bar (let [[a b] (bar-slots d)] (play db (map t/midi (subvec (:melody d) a b)) beat-s a))
        :melody (play db (map t/midi (:melody d)) beat-s 0)
        :answer (play db (map #(some-> % t/midi) (:answer d)) beat-s 0)))))

(defn- set-note [db i note]
  (let [d (:dict db)]
    (if (editable? d i)
      (let [d (assoc-in d [:answer i] note)
            [_ b] (bar-slots d)
            nxt (first (filter #(editable? d %) (range (inc i) b)))]
        (assoc db :dict (assoc d :sel (or nxt i))))
      db)))

(rf/reg-event-db :dict/pick
  (fn [db [_ i step]]
    (let [{:keys [key beats answer] :as d} (:dict db)]
      (if (editable? d i)
        (set-note db i {:s step :a (t/implied-alt key beats answer i step)})
        db))))

(rf/reg-event-db :dict/select
  (fn [db [_ i]] (if (editable? (:dict db) i) (assoc-in db [:dict :sel] i) db)))

(defn- ref-step
  "The note before the selection, or the middle of the range."
  [{:keys [answer sel] [lo hi] :range}]
  (or (:s (last (keep #(get answer %) (range 0 (inc sel)))))
      (quot (+ lo hi) 2)))

(rf/reg-event-fx :dict/letter
  (fn [{:keys [db]} [_ l]]
    (let [{:keys [key beats answer sel] :as d} (:dict db)
          ref (ref-step (assoc d :sel (if (get answer sel) sel (dec sel))))
          [lo hi] (:range d)
          s (->> (range (- ref 3) (+ ref 4))
                 (filter #(= l (mod % 7)))
                 first)
          s (cond (< s lo) (+ s 7) (> s hi) (- s 7) :else s)
          note {:s s :a (t/implied-alt key beats answer sel s)}]
      (when (editable? d sel)
        {:db (set-note db sel note)
         :audio/tap (t/midi note)}))))

(rf/reg-event-fx :dict/midi
  (fn [{:keys [db]} [_ m]]
    (let [{:keys [key sel] :as d} (:dict db)]
      (when (and d (editable? d sel))
        {:db (set-note db sel (t/spell key m))}))))

(defn edit-target
  "The selected slot, or the note just entered when the selection is still empty."
  [{:keys [answer sel] :as d} op]
  (if (and (editable? d sel) (nil? (get answer sel)) (not (#{:delete :left :right} op))
           (editable? d (dec sel)) (get answer (dec sel)))
    (dec sel)
    sel))

(rf/reg-event-fx :dict/edit
  (fn [{:keys [db]} [_ op]]
    (let [{:keys [key beats answer] :as d} (:dict db)
          sel (edit-target d op)
          note (get answer sel)
          moved (fn [ds] (let [s (+ (:s note) ds)] {:s s :a (t/implied-alt key beats answer sel s)}))
          note' (when note
                  (case op
                    :up (moved 1)
                    :down (moved -1)
                    :sharp (update note :a #(min 2 (inc %)))
                    :flat (update note :a #(max -2 (dec %)))
                    :natural (assoc note :a 0)
                    :respell (t/respell note)
                    nil))]
      (when (editable? d sel)
        (case op
          :delete {:db (-> db
                           (assoc-in [:dict :answer sel] nil)
                           (update :dict (fn [d] (let [[a] (bar-slots d)
                                                       prev (dec sel)]
                                                   (if (and (nil? note) (>= prev a) (editable? d prev))
                                                     (assoc-in (assoc d :sel prev) [:answer prev] nil)
                                                     d)))))}
          (:left :right) (let [i (+ sel (if (= op :left) -1 1))]
                           (when (editable? d i) {:db (assoc-in db [:dict :sel] i)}))
          (when note'
            {:db (assoc-in db [:dict :answer sel] note')
             :audio/tap (t/midi note')}))))))

(rf/reg-event-fx :dict/check
  (fn [{:keys [db]} _]
    (let [d (:dict db)]
      (when (and d (= :write (:phase d)) (not (contains? (:checked d) (:bar d))))
        (let [d (update d :checked conj (:bar d))
              last? (= (:bar d) (dec (:bars d)))
              d (cond-> d last? (assoc :phase :done :sel nil))
              entry (when last?
                      (merge (select-keys d [:key :beats :bars :clef :chromatic :repeats :t]) (results d)))]
          {:db (cond-> (assoc db :dict d)
                 entry (update :history #(vec (take-last max-history (conj % entry)))))})))))

(rf/reg-event-fx :dict/next
  (fn [{:keys [db]} _]
    (let [d (:dict db)]
      (when (and d (= :write (:phase d)) (contains? (:checked d) (:bar d)))
        (let [d (assoc d :bar (inc (:bar d)) :plays 0)
              d (assoc d :sel (first-free d))]
          {:db (assoc db :dict d)
           :dispatch [:dict/play]})))))

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
                       :else (when-let [op ({"ArrowUp" :up "ArrowDown" :down "ArrowLeft" :left "ArrowRight" :right
                                             "#" :sharp "-" :flat "n" :natural "N" :natural "Backspace" :delete
                                             "Delete" :delete "x" :respell} k)]
                               {:dispatch [:dict/edit op]})))
        nil))))

;; ---------------------------------------------------------------- stats

(rf/reg-event-db :stats/clear
  (fn [db [_ which]] (assoc db which [])))
