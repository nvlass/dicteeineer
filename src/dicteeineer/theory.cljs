(ns dicteeineer.theory)

;; A note is {:s diatonic step, :a alteration}: :s is letter index + 7 × octave, so C4 = 28.
;; A key is {:tonic {:l letter :a alteration} :mode :major|:minor}; minor melodies use the
;; harmonic minor scale, while the signature is the natural minor one.

(def letters ["C" "D" "E" "F" "G" "A" "B"])
(def natural-pc [0 2 4 5 7 9 11])
(def letter-fifths [0 2 4 -1 1 3 5])

(defn letter [{:keys [s]}] (mod s 7))
(defn octave [{:keys [s]}] (Math/floor (/ s 7)))

(defn midi [{:keys [s a]}]
  (+ 12 (* 12 (Math/floor (/ s 7))) (natural-pc (mod s 7)) a))

(def acc-sym {-2 "𝄫" -1 "♭" 0 "" 1 "♯" 2 "𝄪"})

(defn note-name
  ([n] (note-name n letters))
  ([n names] (str (names (letter n)) (acc-sym (:a n)) (octave n))))

(defn pc-name [n names] (str (names (letter n)) (acc-sym (:a n))))

(defn tonic-name [{:keys [l a]} names] (str (names l) (acc-sym a)))

;; ---------------------------------------------------------------- keys

(def scales {:major [0 2 4 5 7 9 11] :minor [0 2 3 5 7 8 11]})

(def default-key {:tonic {:l 0 :a 0} :mode :major})

(defn signature
  "Sharps (positive) or flats (negative) in the key signature."
  [{:keys [tonic mode]}]
  (+ (letter-fifths (:l tonic)) (* 7 (:a tonic)) (if (= mode :minor) -3 0)))

(defn keys-of [mode]
  (sort-by signature
           (for [a [-1 0 1] l (range 7)
                 :let [k {:tonic {:l l :a a} :mode mode}]
                 :when (<= (Math/abs (signature k)) 6)]
             k)))

(defn tonic-pc [{:keys [tonic]}] (mod (+ (natural-pc (:l tonic)) (:a tonic)) 12))

(def sharp-order [3 0 4 1 5 2 6])

(defn sig-alt
  "The alteration the key signature gives letter `l`."
  [key l]
  (let [n (signature key)]
    (cond (pos? n) (if (some #{l} (take n sharp-order)) 1 0)
          (neg? n) (if (some #{l} (take (- n) (reverse sharp-order))) -1 0)
          :else 0)))

(defn- signed [d] (let [d (mod d 12)] (if (> d 6) (- d 12) d)))

(defn scale-alt
  "The alteration that puts letter `l` in the key's scale."
  [{:keys [tonic mode] :as key} l]
  (let [deg (mod (- l (:l tonic)) 7)]
    (signed (- (+ (tonic-pc key) ((scales mode) deg)) (natural-pc l)))))

(defn in-scale? [key n] (= (:a n) (scale-alt key (letter n))))

(defn degree-note
  "Scale degree `d` (0 = tonic, may be negative or past 7) counted from the tonic at step `t`."
  [key t d]
  (let [s (+ t d)] {:s s :a (scale-alt key (mod s 7))}))

(defn tonic-steps [{:keys [tonic]} lo hi]
  (filter #(= (:l tonic) (mod % 7)) (range lo (inc hi))))

(defn spell
  "Spells a MIDI pitch: the key's scale first, then its signature, then sharps or flats to match the signature."
  [key m]
  (let [est (Math/floor (/ (* 7 (- m 12)) 12))
        flat-key? (neg? (signature key))
        cost (fn [{:keys [s a]}]
               (cond (= a (scale-alt key (mod s 7))) 0
                     (= a (sig-alt key (mod s 7))) 0.5
                     (if flat-key? (pos? a) (neg? a)) 2
                     :else 1))]
    (->> (for [s (range (- est 2) (+ est 3))
               :let [a (- m (midi {:s s :a 0}))]
               :when (<= -1 a 1)]
           {:s s :a a})
         (sort-by cost)
         first)))

(defn respell
  "The next enharmonic spelling of `n` (within one sharp or flat), cycling back to `n`."
  [n]
  (let [m (midi n)
        opts (sort-by :s (for [s (range (- (:s n) 2) (+ (:s n) 3))
                               :let [a (- m (midi {:s s :a 0}))]
                               :when (<= -1 a 1)]
                           {:s s :a a}))
        i (first (keep-indexed #(when (= n %2) %1) opts))]
    (if i (nth opts (mod (inc i) (count opts))) (first opts))))

;; ---------------------------------------------------------------- intervals

(def major-ref [0 2 4 5 7 9 11])

(defn interval
  "Interval between two notes regardless of order: :g letter distance (0 = unison), :g7 reduced, :q quality."
  [x y]
  (let [[lo hi] (if (or (< (:s y) (:s x)) (and (= (:s x) (:s y)) (< (midi y) (midi x)))) [y x] [x y])
        g (- (:s hi) (:s lo))
        g7 (mod g 7)
        semis (- (midi hi) (midi lo))
        d (- semis (major-ref g7) (* 12 (quot g 7)))
        q (if (#{0 3 4} g7)
            (case d 0 :P 1 :A -1 :d (if (pos? d) :AA :dd))
            (case d 0 :M -1 :m 1 :A -2 :d (if (pos? d) :AA :dd)))]
    {:g g :g7 g7 :num (inc g) :semis semis :q q}))

;; ---------------------------------------------------------------- clefs and ranges

(def ranges
  "Readable step ranges per clef: narrow is about an octave, wide adds ledger lines."
  {:treble {:narrow [28 35] :wide [26 42]}
   :bass {:narrow [21 28] :wide [14 30]}
   :grand {:narrow [21 42] :wide [14 42]}})

(defn step-range [clef width] (get-in ranges [clef width]))

(defn note-range
  "MIDI range for single notes: the clef's middle octave, or C3–B5."
  [clef octaves]
  (if (= octaves 3) [48 83] (if (= clef :bass) [48 59] [60 71])))

(defn single-clef [clef octaves] (if (= octaves 3) :grand clef))

(defn white? [m] (not (#{1 3 6 8 10} (mod m 12))))

;; ---------------------------------------------------------------- bars

(defn accidentals
  "The accidental to print for each note, or nil: what differs from the signature and from earlier
   notes on the same step in the bar."
  [key beats notes]
  (loop [i 0 state {} out []]
    (if (= i (count notes))
      out
      (let [state (if (zero? (mod i beats)) {} state)
            n (nth notes i)]
        (if-not n
          (recur (inc i) state (conj out nil))
          (let [cur (get state (:s n) (sig-alt key (letter n)))]
            (recur (inc i) (assoc state (:s n) (:a n)) (conj out (when (not= cur (:a n)) (:a n))))))))))

(defn implied-alt
  "What an unmarked note on step `s` at `slot` means: the signature, or an earlier accidental in the bar."
  [key beats notes slot s]
  (let [v (vec notes)
        end (min slot (count v))
        start (min end (* beats (quot slot beats)))]
    (or (some (fn [n] (when (= s (:s n)) (:a n))) (reverse (subvec v start end)))
        (sig-alt key (mod s 7)))))
