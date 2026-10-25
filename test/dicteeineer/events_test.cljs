(ns dicteeineer.events-test
  (:require [cljs.test :refer [deftest is]]
            [dicteeineer.events :as e]))

(def c4 {:s 28 :a 0})
(def d4 {:s 29 :a 0})

(defn- ev [n v] {:n n :v v :dot false :trip false})

(def dict
  {:beats 4 :bars 2 :phase :write :bar 0 :checked #{} :given? true :repeats :strict :plays 0
   :answer [[(ev c4 :quarter) (ev d4 :half)] []]
   :sel 2})

(deftest editing
  (is (not (e/editable? dict 0)) "given")
  (is (e/editable? dict 1))
  (is (not (e/editable? dict 2)) "the append position isn't a note")
  (is (not (e/editable? (update dict :checked conj 0) 1)) "checked bar")
  (is (= 1 (e/edit-target dict)) "the cursor past the last note edits it")
  (is (= 1 (e/edit-target (assoc dict :sel 1))))
  (is (nil? (e/edit-target (assoc-in dict [:answer 0] [(ev c4 :quarter)]))) "only the given note"))

(deftest plays
  (is (e/can-play? (assoc dict :plays 1)))
  (is (not (e/can-play? (assoc dict :plays 2))))
  (is (e/can-play? (assoc dict :repeats :free :plays 9))))

(deftest note-pools
  (is (= [60 62 64 65 67 69 71] (e/note-pool {:clef :treble :note-octaves 1 :note-accidentals false})))
  (is (= 12 (count (e/note-pool {:clef :bass :note-octaves 1 :note-accidentals true}))))
  (is (= 36 (count (e/note-pool {:clef :treble :note-octaves 3 :note-accidentals true})))))
