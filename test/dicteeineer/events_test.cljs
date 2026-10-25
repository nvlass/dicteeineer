(ns dicteeineer.events-test
  (:require [cljs.test :refer [deftest is]]
            [dicteeineer.theory :as t]
            [dicteeineer.events :as e]))

(def c4 {:s 28 :a 0})
(def d4 {:s 29 :a 0})
(def f#4 {:s 31 :a 1})
(def gb4 {:s 32 :a -1})

(def dict
  {:key t/default-key :beats 4 :bars 2 :phase :write :bar 0 :checked #{} :given #{0}
   :melody [c4 d4 f#4 c4 d4 d4 d4 c4]
   :answer [c4 d4 gb4 d4 nil nil nil nil]})

(deftest grading
  (is (= [:ok :ok :enh :wrong :wrong] (map #(e/grade dict %) (range 5))))
  (is (= {:n 7 :ok 1 :enh 1} (e/results dict)) "the given first note doesn't count"))

(deftest editing
  (is (not (e/editable? dict 0)) "given")
  (is (e/editable? dict 3))
  (is (not (e/editable? dict 4)) "next bar")
  (is (not (e/editable? (update dict :checked conj 0) 3)) "checked bar")
  (is (= 4 (e/edit-target (assoc dict :sel 4) :sharp)) "outside the bar stays put")
  (let [d (assoc-in (assoc dict :sel 3) [:answer 3] nil)]
    (is (= 2 (e/edit-target d :sharp)) "an empty selection edits the note just entered")
    (is (= 3 (e/edit-target d :delete)))))

(deftest plays
  (is (e/can-play? (assoc dict :repeats :strict :plays 1)))
  (is (not (e/can-play? (assoc dict :repeats :strict :plays 2))))
  (is (e/can-play? (assoc dict :repeats :free :plays 9))))

(deftest note-pools
  (is (= [60 62 64 65 67 69 71] (e/note-pool {:clef :treble :note-octaves 1 :note-accidentals false})))
  (is (= 12 (count (e/note-pool {:clef :bass :note-octaves 1 :note-accidentals true}))))
  (is (= 36 (count (e/note-pool {:clef :treble :note-octaves 3 :note-accidentals true})))))
