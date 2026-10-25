(ns dicteeineer.rhythm-test
  (:require [cljs.test :refer [deftest is]]
            [dicteeineer.rhythm :as rh]))

(def c4 {:s 28 :a 0})
(def d4 {:s 29 :a 0})
(def e4 {:s 30 :a 0})
(def b#3 {:s 27 :a 1})

(defn- ev [n v & flags] (merge {:n n :v v :dot false :trip false} (zipmap flags (repeat true))))

(deftest durations
  (is (= 18 (rh/ticks (ev c4 :quarter :dot))))
  (is (= 4 (rh/ticks (ev c4 :eighth :trip))))
  (is (= 48 (rh/total [(ev c4 :half :dot) (ev c4 :quarter)])))
  (is (= [0 6 12] (rh/onsets [(ev c4 :eighth) (ev c4 :eighth) (ev c4 :half)]))))

(deftest grading
  (let [want [(ev c4 :quarter :dot) (ev d4 :eighth) (ev e4 :half)]]
    (is (= {:marks [:ok :ok :ok] :n 3 :ok 3 :enh 0 :pitch 3 :rhythm 3} (rh/grade-bar want want #{})))
    (let [r (rh/grade-bar want [(ev c4 :quarter) (ev d4 :quarter) (ev e4 :half)] #{})]
      (is (= [:rhythm :rhythm :ok] (:marks r)) "D starts too early: its pitch is right but it's off the beat")
      (is (= {:n 3 :ok 1 :pitch 2 :rhythm 1} (select-keys r [:n :ok :pitch :rhythm]))))
    (let [r (rh/grade-bar want [(ev b#3 :quarter :dot) (ev e4 :eighth) (ev e4 :half)] #{0})]
      (is (= [:enh :pitch :ok] (:marks r)))
      (is (= {:n 2 :ok 1 :enh 0 :pitch 1 :rhythm 2} (dissoc r :marks)) "the given first note is skipped"))))

(deftest groups
  (let [bar [(ev c4 :eighth) (ev c4 :eighth) (ev c4 :quarter) (ev c4 :eighth :dot) (ev c4 :16th)
             (ev c4 :eighth) (ev c4 :16th) (ev c4 :16th)]]
    (is (= [[0 1] [3 4] [5 6 7]] (rh/beam-groups bar))))
  (is (= [[0] [2]] (rh/beam-groups [(ev c4 :eighth) (ev c4 :quarter) (ev c4 :eighth) (ev c4 :half)])) "lone eighths get flags")
  (let [trip (ev c4 :eighth :trip)]
    (is (= [[1 2 3] [4 5 6]] (rh/triplet-groups [(ev c4 :quarter) trip trip trip trip trip trip])))))
