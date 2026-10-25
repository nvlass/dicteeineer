(ns dicteeineer.theory-test
  (:require [cljs.test :refer [deftest is testing]]
            [dicteeineer.theory :as t]))

(def c-major t/default-key)
(def a-minor {:tonic {:l 5 :a 0} :mode :minor})
(def e-flat {:tonic {:l 2 :a -1} :mode :major})

(deftest pitches
  (is (= 60 (t/midi {:s 28 :a 0})))
  (is (= 36 (t/midi {:s 14 :a 0})))
  (is (= 59 (t/midi {:s 28 :a -1})))
  (is (= "C♯4" (t/note-name {:s 28 :a 1}))))

(deftest signatures
  (is (= 0 (t/signature c-major)))
  (is (= 0 (t/signature a-minor)))
  (is (= -3 (t/signature e-flat)))
  (is (= 13 (count (t/keys-of :major))))
  (is (= 13 (count (t/keys-of :minor))))
  (is (= [0 0 -1 0 0 -1 -1] (map #(t/sig-alt e-flat %) (range 7)))))

(deftest scales
  (testing "harmonic minor raises the 7th"
    (is (= 1 (t/scale-alt a-minor 4)))
    (is (= 0 (t/sig-alt a-minor 4))))
  (is (= [60 62 64 65 67 69 71 72] (map #(t/midi (t/degree-note c-major 28 %)) (range 8)))))

(deftest spelling
  (is (= {:s 28 :a 0} (t/spell c-major 60)))
  (is (= {:s 30 :a -1} (t/spell e-flat 63)))
  (is (= {:s 32 :a 1} (t/spell a-minor 68)))
  (is (= 61 (t/midi (t/respell {:s 28 :a 1}))))
  (is (not= {:s 28 :a 1} (t/respell {:s 28 :a 1}))))

(deftest bar-accidentals
  (let [f# {:s 31 :a 1} f {:s 31 :a 0} g {:s 32 :a 0}]
    (is (= [1 nil 0 nil] (t/accidentals c-major 4 [f# f# f g])))
    (is (= [1 nil nil nil 1] (t/accidentals c-major 4 [f# g g g f#])) "a new bar resets")
    (is (= 1 (t/implied-alt c-major 4 [f# g] 2 31)))
    (is (= 0 (t/implied-alt c-major 4 [f# g g g] 4 31)))))
