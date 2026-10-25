(ns dicteeineer.melody
  (:require [dicteeineer.theory :as t]))

;; Melodies are walks over scale degrees: mostly steps, some leaps, each large leap recovered by a
;; step back; start on a tonic triad note and end 2→1 or 7→1. Candidates failing `melodic-ok?`
;; are retried. Chromatic notes are added afterwards as half-step approaches to the next note,
;; spelled by direction (raised going up, lowered going down).

(def moves [[1 26] [-1 26] [2 10] [-2 10] [0 3] [3 5] [-3 5] [4 4] [-4 4] [5 1] [-5 1] [7 1] [-7 1]])

(defn- pick [rnd pairs]
  (let [total (reduce + (map second pairs))
        r (* (rnd) total)]
    (loop [[[x w] & more] pairs acc 0]
      (if (or (empty? more) (< r (+ acc w))) x (recur more (+ acc w))))))

(defn melodic-ok?
  "No augmented or diminished leaps (tritones, augmented 2nds), no 7ths, nothing past an octave."
  [x y]
  (let [{:keys [g q]} (t/interval x y)]
    (and (<= g 7) (not= g 6) (or (zero? g) (#{:P :M :m} q)))))

(defn- chromatic-ok? [x y]
  (let [{:keys [g q]} (t/interval x y)]
    (or (and (= g 0) (= q :A))
        (and (<= 1 g 2) (#{:M :m} q))
        (and (= g 3) (= q :P)))))

(defn tonic-step
  "The tonic nearest the middle of the range, with room for a step into it."
  [key [lo hi]]
  (let [mid (/ (+ lo hi) 2)
        ok (filter #(or (<= lo (dec %)) (<= (inc %) hi)) (t/tonic-steps key lo hi))]
    (first (sort-by (juxt #(Math/abs (- % mid)) identity) ok))))

(defn- playable? [n] (<= 36 (t/midi n) 96))

(defn- walk
  "Degrees from the tonic, or nil when it runs into a corner; `note` maps a degree to a note."
  [rnd n lo hi note]
  (let [ok? (fn [d e] (and (<= lo e hi) (playable? (note e)) (melodic-ok? (note d) (note e))))
        start (for [[d w] [[0 4] [2 3] [4 3] [-3 1] [7 1]] :when (<= lo d hi) :when (playable? (note d))] [d w])]
    (loop [ds [(pick rnd start)]]
      (let [d (peek ds)]
        (if (= (count ds) (- n 2))
          (let [ps (for [p [1 -1] :when (ok? d p) :when (melodic-ok? (note p) (note 0))]
                     [p (if (= (pos? p) (pos? d)) 3 1)])]
            (when (seq ps) (conj ds (pick rnd ps) 0)))
          (let [prev (when (> (count ds) 1) (- d (nth ds (- (count ds) 2))))
                recover? (and prev (>= (Math/abs prev) 3))
                ms (for [[m w] moves
                         :when (ok? d (+ d m))
                         :when (or (not recover?) (and (<= 1 (Math/abs m) 2) (neg? (* m prev))))
                         :when (not (and prev (zero? prev) (zero? m)))]
                     [m w])]
            (when (seq ms) (recur (conj ds (+ d (pick rnd ms)))))))))))

(defn- line-ok? [notes]
  (and (every? (fn [[x y]] (melodic-ok? x y)) (partition 2 1 notes))
       (<= (count (filter (fn [[x y]] (= x y)) (partition 2 1 notes))) (quot (count notes) 8))))

(defn- chromaticize [rnd key notes bars]
  (let [n (count notes)]
    (loop [i 1 out notes budget (max 1 (quot bars 2))]
      (if (or (>= i (- n 2)) (zero? budget))
        out
        (let [y (out (inc i))
              cands (for [ds [-1 1]
                          :let [c {:s (+ (:s y) ds) :a 0}
                                c (assoc c :a (- (+ (t/midi y) ds) (t/midi c)))]
                          :when (<= -1 (:a c) 1)
                          :when (not (t/in-scale? key c))
                          :when (playable? c)
                          :when (chromatic-ok? (out (dec i)) c)]
                      c)]
          (if (and (seq cands) (< (rnd) 0.3))
            (recur (+ i 2) (assoc out i (nth cands (Math/floor (* (rnd) (count cands))))) (dec budget))
            (recur (inc i) out budget)))))))

(defn generate
  "A melody of `bars` × `beats` quarter notes in `key`, within step range `[lo hi]`."
  ([opts] (generate opts rand))
  ([{:keys [key beats bars chromatic] [lo hi :as span] :range} rnd]
   (let [n (* beats bars)
         t (tonic-step key span)
         note #(t/degree-note key t %)
         line (or (some (fn [_] (when-let [ds (walk rnd n (- lo t) (- hi t) note)]
                                  (let [notes (mapv note ds)] (when (line-ok? notes) notes))))
                        (range 400))
                  (mapv note (take n (cycle [0 1 2 1]))))]
     (if chromatic (chromaticize rnd key line bars) line))))

(defn scale-notes
  "The scale up an octave and back, from the tonic that keeps it most inside the range."
  [key [lo hi]]
  (let [overflow #(+ (max 0 (- lo %)) (max 0 (- (+ % 7) hi)))
        t (first (sort-by (juxt overflow identity) (t/tonic-steps key (- lo 6) hi)))]
    (mapv #(t/degree-note key t %) (concat (range 8) (range 6 -1 -1)))))
