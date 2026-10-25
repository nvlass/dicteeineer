(ns dicteeineer.rhythm
  (:require [dicteeineer.theory :as t]))

;; Durations are ticks, 12 to the quarter, so eighth triplets (4) and sixteenths (3) are whole numbers.
;; A note event is {:n note :v value :dot bool :trip bool}. Bars are vectors of events; no rests.

(def values {:whole 48 :half 24 :quarter 12 :eighth 6 :16th 3})

(def per-beat 12)

(defn ticks [{:keys [v dot trip]}]
  (let [t (values v)]
    (cond dot (quot (* 3 t) 2)
          trip (quot (* 2 t) 3)
          :else t)))

(defn bar-ticks [beats] (* beats per-beat))

(defn total [bar] (reduce + (map ticks bar)))

(defn onsets [bar] (vec (butlast (reductions + 0 (map ticks bar)))))

(defn can-dot? [v] (not= v :16th))

;; ---------------------------------------------------------------- generation

(defn- ev [v & flags] (merge {:v v :dot false :trip false} (zipmap flags (repeat true))))

(def cells
  "Rhythm cells, the level that introduces them, and where in the bar they may start."
  [{:level 1 :notes [(ev :quarter)]}
   {:level 2 :notes [(ev :half)] :starts #{:strong}}
   {:level 2 :notes [(ev :half :dot)] :starts #{0}}
   {:level 2 :notes [(ev :whole)] :starts #{0}}
   {:level 3 :notes [(ev :eighth) (ev :eighth)]}
   {:level 4 :notes [(ev :quarter :dot) (ev :eighth)] :starts #{:strong}}
   {:level 5 :notes [(ev :16th) (ev :16th) (ev :16th) (ev :16th)]}
   {:level 5 :notes [(ev :eighth) (ev :16th) (ev :16th)]}
   {:level 5 :notes [(ev :16th) (ev :16th) (ev :eighth)]}
   {:level 5 :notes [(ev :eighth :dot) (ev :16th)]}
   {:level 6 :notes [(ev :eighth :trip) (ev :eighth :trip) (ev :eighth :trip)]}])

(def max-level 6)

(defn level-values
  "Durations offered for input at a level, shortest first."
  [level]
  (cond-> []
    (>= level 5) (conj :16th)
    (>= level 3) (conj :eighth)
    true (conj :quarter)
    (>= level 2) (conj :half :whole)))

(defn- fits? [{:keys [notes starts]} beats at]
  (let [len (total notes)
        bar (bar-ticks beats)]
    (and (<= (+ at len) bar)
         (cond (nil? starts) true
               (= starts #{:strong}) (if (= beats 4) (zero? (mod at 24)) (zero? (mod at per-beat)))
               :else (contains? starts at)))))

(defn- pick [rnd pairs]
  (let [total (reduce + (map second pairs))
        r (* (rnd) total)]
    (loop [[[x w] & more] pairs acc 0]
      (if (or (empty? more) (< r (+ acc w))) x (recur more (+ acc w))))))

(defn- weight [level {cl :level}]
  (cond (= cl level) 4 (= cl 1) 2 :else 2))

(defn- fill
  "Cells from `at` up to `end` ticks, drawn from those the level allows."
  [rnd beats level at end]
  (loop [at at out []]
    (if (>= at end)
      out
      (let [ok (for [c cells
                     :when (<= (:level c) level)
                     :when (fits? c beats at)
                     :when (<= (+ at (total (:notes c))) end)]
                 [c (weight level c)])
            c (pick rnd ok)]
        (recur (+ at (total (:notes c))) (into out (:notes c)))))))

(defn- final-bar
  "The last bar ends on a long note once there are half notes."
  [rnd beats level]
  (let [bar (bar-ticks beats)]
    (if (< level 2)
      (fill rnd beats level 0 bar)
      (pick rnd (if (= beats 4)
                  [[(conj (fill rnd beats level 0 24) (ev :half)) 3] [[(ev :whole)] 1]]
                  [[(conj (fill rnd beats level 0 12) (ev :half)) 2] [[(ev :half :dot)] 1]])))))

(defn bars
  "Rhythms for `n` bars of `beats` quarters at `level`."
  [rnd beats n level]
  (conj (vec (repeatedly (dec n) #(fill rnd beats level 0 (bar-ticks beats))))
        (final-bar rnd beats level)))

;; ---------------------------------------------------------------- grading

(defn- midi-eq [x y] (= (t/midi x) (t/midi y)))

(defn grade-bar
  "Marks for each answer note (:ok :enh :rhythm :pitch :wrong) and counts over the wanted notes,
   skipping wanted indices in `skip`. A note off the wanted onsets is a rhythm mistake when its pitch
   is the one sounding or the one coming next."
  [want got skip]
  (let [w-on (onsets want)
        g-on (onsets got)
        at (zipmap w-on (range))
        sounding (fn [o] (last (keep-indexed #(when (<= %2 o) %1) w-on)))
        mark (fn [gi g]
               (let [o (g-on gi)]
                 (if-let [wi (at o)]
                   (let [w (want wi)
                         p? (midi-eq (:n w) (:n g))
                         r? (= (ticks w) (ticks g))]
                     (cond (and p? r?) (if (= (:n w) (:n g)) :ok :enh)
                           p? :rhythm
                           r? :pitch
                           :else :wrong))
                   (let [wi (sounding o)
                         near (keep #(get want %) [wi (when wi (inc wi))])]
                     (if (some #(midi-eq (:n %) (:n g)) near) :rhythm :wrong)))))
        by-onset (zipmap g-on got)
        res (for [wi (range (count want))
                  :when (not (skip wi))
                  :let [w (want wi) h (get by-onset (w-on wi))
                        p? (boolean (and h (midi-eq (:n w) (:n h))))
                        r? (boolean (and h (= (ticks w) (ticks h))))]]
              {:p p? :r r? :exact (and p? r? (= (:n w) (:n h)))})]
    {:marks (vec (map-indexed mark got))
     :n (count res)
     :ok (count (filter :exact res))
     :enh (count (filter #(and (:p %) (:r %) (not (:exact %))) res))
     :pitch (count (filter :p res))
     :rhythm (count (filter :r res))}))

;; ---------------------------------------------------------------- notation groups

(defn beam-groups
  "Index runs of eighths and shorter that share a beat; runs of one get flags instead."
  [bar]
  (let [on (onsets bar)]
    (->> (range (count bar))
         (partition-by (fn [i] (if (#{:eighth :16th} (:v (bar i)))
                                 [:beam (quot (on i) per-beat)]
                                 [:single i])))
         (filter #(#{:eighth :16th} (:v (bar (first %)))))
         (map vec))))

(defn triplet-groups
  "Index runs of triplet notes, cut where they complete a group (two of their plain value)."
  [bar]
  (loop [i 0 cur [] acc 0 out []]
    (if (= i (count bar))
      (cond-> out (seq cur) (conj cur))
      (let [e (bar i)]
        (if (:trip e)
          (let [acc (+ acc (ticks e))
                cur (conj cur i)
                group (* 2 (values (:v (bar (first cur)))))]
            (if (zero? (mod acc group))
              (recur (inc i) [] 0 (conj out cur))
              (recur (inc i) cur acc out)))
          (recur (inc i) [] 0 (cond-> out (seq cur) (conj cur))))))))
