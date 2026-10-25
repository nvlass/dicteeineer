(ns dicteeineer.i18n.en
  (:require [clojure.string :as str]
            [dicteeineer.theory :as t]))

(defn- note [loc n] (t/note-name n (:names loc)))
(defn- tonic [loc tn] (t/tonic-name tn (:names loc)))
(defn- pc [loc p] (t/pc-name (t/spell t/default-key (+ 60 p)) (:names loc)))
(defn- key-name [loc {tn :tonic mode :mode}] (str (tonic loc tn) (if (= mode :minor) " minor" " major")))
(defn- plural [n one many] (str n " " (if (= n 1) one many)))

(def dict
  {:fmt
   {:note note
    :tonic tonic
    :pc pc
    :signature (fn [_ s] (cond (zero? s) "no sharps or flats" (pos? s) (str s " ♯") :else (str (- s) " ♭")))
    :note-summary (fn [_ {:keys [note-octaves note-accidentals note-reference]}]
                    (str/join " · " [(plural note-octaves "octave" "octaves")
                                     (if note-accidentals "all 12 notes" "white keys")
                                     (if (= note-reference :none) "no reference" "reference")]))
    :dict-summary (fn [loc {:keys [key beats bars chromatic repeats]}]
                    (str/join " · " (cond-> [(key-name loc key) (str beats "/4") (plural bars "bar" "bars")]
                                      chromatic (conj "chromatic")
                                      true (conj (if (= repeats :free) "free" "strict")))))
    :wrong-note (fn [_ target answer] [:<> "It was " [:b target] ", you played " [:b answer]])
    :result (fn [_ ok enh n] (str ok (when (pos? enh) (str " + " enh " spelled differently")) " of " n " notes right"))
    :bar-of (fn [_ b n] (str "Bar " b " of " n))
    :hearing (fn [_ b] (str " · plays bars " (dec b) "–" b))
    :plays-left (fn [_ n] (plural n "play left" "plays left"))
    :bar-wrong (fn [_ n] (plural n "mistake" "mistakes"))
    :attempts (fn [_ n] (plural n "attempt" "attempts"))}

   :nav {:note "Note" :dictation "Dictation" :stats "Stats" :settings "Settings"}

   :ui
   {:loading-sounds "loading sounds…"
    :samples-failed "Couldn't load the piano samples."
    :language "Language"
    :note-names "Note names"
    :solfege-sample "Do Re Mi"
    :clef "Clef"
    :treble "Treble" :bass "Bass" :grand "Grand staff"
    :range "Range"
    :narrow "Narrow" :wide "Wide"
    :midi "MIDI keyboard"
    :midi-hint "Answer by playing on a connected MIDI keyboard."
    :midi-none "No MIDI inputs found."
    :midi-unsupported "This browser has no Web MIDI (Safari)."
    :midi-denied "MIDI access was denied."
    :options "Options"
    :octaves "Octaves"
    :one-octave "1 octave" :three-octaves "3 octaves"
    :reference "Reference note"
    :none "None"
    :reference-hint "With a reference it plays before the first note and stays on a button. Without one this is absolute-pitch training."
    :accidentals "Accidentals"
    :accidentals-hint "All 12 notes instead of the white keys."
    :auto-next "Next note right after a correct answer"
    :note-intro "A note plays: find it on the keyboard."
    :which-note "Which note was it?"
    :correct "Correct:"
    :start "Start"
    :replay "🔊 Again"
    :reference-btn (fn [_ name] (str "Reference " name))
    :hear-yours "Hear yours"
    :next "Next"
    :note-keys [:<> [:kbd "Enter"] " start / next, " [:kbd "Space"] " replay, " [:kbd "R"] " reference."]
    :key "Key"
    :major "major" :minor "minor"
    :meter "Time"
    :bars "Bars"
    :repeats "Repeats"
    :strict "Strict (2 plays)" :free "Free"
    :chromatic "Chromatic notes"
    :chromatic-hint "Notes outside the key, as half-step approaches: raised going up, lowered going down."
    :first-given "Give the first note"
    :start-dictation "Start dictation"
    :listen-scale "Listen: the scale, up and down."
    :skip "Skip"
    :play-step "▶ Play"
    :hear-bar "▶ This bar"
    :hear-scale "Scale"
    :check "Check"
    :next-bar "Next bar"
    :bar-correct "All correct"
    :hear-melody "▶ Melody"
    :hear-answer "▶ Your answer"
    :new-dictation "New dictation"
    :quit "Stop"
    :settings-back "Settings"
    :tool-up "Step up (↑)"
    :tool-down "Step down (↓)"
    :tool-sharp "Sharp (#)"
    :tool-flat "Flat (-)"
    :tool-natural "Natural (n)"
    :tool-respell "Enharmonic spelling (x)"
    :tool-delete "Delete (⌫)"
    :dict-keys [:<> "Write each bar after hearing it with the bar before. Click the staff, play the keyboard, or type "
                [:kbd "A"] "–" [:kbd "G"] "; " [:kbd "↑"] [:kbd "↓"] " move a note, " [:kbd "#"] " " [:kbd "-"] " " [:kbd "n"]
                " accidentals, " [:kbd "x"] " respell, " [:kbd "⌫"] " delete, " [:kbd "Space"] " play, "
                [:kbd "Enter"] " check / next."]
    :no-attempts "No answers yet."
    :no-dictations "No finished dictations yet."
    :last-20 "last 20"
    :by-note "By note"
    :by-octave "By octave"
    :confusions "Most confused"
    :clear-stats "Clear these stats"
    :sure "Delete for good?"
    :clear "Delete"
    :cancel "Cancel"}})
