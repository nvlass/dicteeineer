(ns dicteeiner.melody-test
  (:require [cljs.test :refer [deftest is testing]]
            [dicteeiner.theory :as t]
            [dicteeiner.melody :as m]))

(defn- keys-all [] (concat (t/keys-of :major) (t/keys-of :minor)))

(defn- check [{:keys [key beats bars range chromatic] :as opts}]
  (let [line (m/generate opts)
        [lo hi] range
        final (peek line)
        penult (line (- (count line) 2))]
    (is (= (* beats bars) (count line)))
    (is (every? #(<= (dec lo) (:s %) (inc hi)) line) "within range")
    (is (= (get-in key [:tonic :l]) (t/letter final)) (str "ends on the tonic " (pr-str key range (map t/note-name line))))
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
    (dotimes [_ 3]
      (check {:key key :beats (rand-nth [3 4]) :bars (rand-nth [4 8 12])
              :range (t/step-range clef width) :chromatic chromatic}))))

(deftest leaps
  (dotimes [_ 200]
    (let [line (m/generate {:key t/default-key :beats 4 :bars 8 :range [28 35]})]
      (is (every? (fn [[x y]] (m/melodic-ok? x y)) (partition 2 1 line))))))

(deftest chromatic-shows-up
  (is (some #(not (t/in-scale? t/default-key %))
            (mapcat (fn [_] (m/generate {:key t/default-key :beats 4 :bars 8 :range [26 42] :chromatic true}))
                    (range 20)))))

(deftest scale-up-and-down
  (let [s (m/scale-notes t/default-key [28 35])]
    (is (= [60 62 64 65 67 69 71 72 71 69 67 65 64 62 60] (map t/midi s)))))
