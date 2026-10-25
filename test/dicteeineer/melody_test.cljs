(ns dicteeineer.melody-test
  (:require [cljs.test :refer [deftest is]]
            [dicteeineer.theory :as t]
            [dicteeineer.rhythm :as rh]
            [dicteeineer.melody :as m]))

(defn- keys-all [] (concat (t/keys-of :major) (t/keys-of :minor)))

(defn- check [{:keys [key beats bars range chromatic rhythm] :as opts}]
  (let [bs (m/generate opts)
        line (mapv :n (apply concat bs))
        [lo hi] range
        final (peek line)
        penult (line (- (count line) 2))]
    (is (= bars (count bs)))
    (is (every? #(= (rh/bar-ticks beats) (rh/total %)) bs) "every bar is full")
    (is (every? #(<= (dec lo) (:s %) (inc hi)) line) "within range")
    (is (= (get-in key [:tonic :l]) (t/letter final)) "ends on the tonic")
    (is (t/in-scale? key final))
    (is (= 1 (Math/abs (- (:s final) (:s penult)))) "final approached by step")
    (is (every? #(<= 36 (t/midi %) 96) line) "inside the samples")
    (if chromatic
      (doseq [[x y] (partition 2 1 line) :when (not (t/in-scale? key x))]
        (is (= 1 (Math/abs (- (t/midi y) (t/midi x)))) "a chromatic note resolves by half step")
        (is (= (compare (t/midi y) (t/midi x)) (compare (:s y) (:s x))) "spelled by direction"))
      (is (every? #(t/in-scale? key %) line) "diatonic"))))

(deftest melodies
  (doseq [key (keys-all)
          clef [:treble :bass :grand]
          width [:narrow :wide]
          chromatic [false true]]
    (dotimes [_ 2]
      (check {:key key :beats (rand-nth [3 4]) :bars (rand-nth [4 8 12]) :rhythm (inc (rand-int 6))
              :range (t/step-range clef width) :chromatic chromatic}))))

(deftest rhythm-levels
  (doseq [level (range 1 7) beats [3 4]]
    (dotimes [_ 30]
      (let [bs (rh/bars rand beats 8 level)
            evs (apply concat bs)
            allowed (set (mapcat :notes (filter #(<= (:level %) level) rh/cells)))]
        (is (every? #(= (rh/bar-ticks beats) (rh/total %)) bs))
        (is (every? allowed evs) (str "level " level " uses only its cells"))
        (when (>= level 2)
          (is (#{:half :whole} (:v (peek (peek bs)))) "ends on a long note"))
        (doseq [g (rh/triplet-groups (vec evs))]
          (is (= 3 (count g)) "triplets come in threes"))))))

(deftest new-cells-show-up
  (doseq [level (range 2 7)]
    (let [evs (set (apply concat (mapcat (fn [_] (rh/bars rand 4 8 level)) (range 10))))
          fresh (set (mapcat :notes (filter #(= (:level %) level) rh/cells)))]
      (is (some fresh evs) (str "level " level)))))

(deftest leaps
  (dotimes [_ 200]
    (let [line (mapv :n (apply concat (m/generate {:key t/default-key :beats 4 :bars 8 :range [28 35]})))]
      (is (every? (fn [[x y]] (m/melodic-ok? x y)) (partition 2 1 line))))))

(deftest chromatic-shows-up
  (is (some #(not (t/in-scale? t/default-key (:n %)))
            (mapcat (fn [_] (apply concat (m/generate {:key t/default-key :beats 4 :bars 8 :range [26 42] :chromatic true})))
                    (range 20)))))

(deftest scale-up-and-down
  (let [s (m/scale-notes t/default-key [28 35])]
    (is (= [60 62 64 65 67 69 71 72 71 69 67 65 64 62 60] (map t/midi s)))))
