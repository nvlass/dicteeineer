(ns dicteeineer.i18n.el
  (:require [clojure.string :as str]
            [dicteeineer.theory :as t]))

(defn- note [loc n] (t/note-name n (:names loc)))
(defn- tonic [loc tn] (t/tonic-name tn (:names loc)))
(defn- pc [loc p] (t/pc-name (t/spell t/default-key (+ 60 p)) (:names loc)))
(defn- key-name [loc {tn :tonic mode :mode}] (str (tonic loc tn) (if (= mode :minor) " ελάσσονα" " μείζονα")))
(defn- plural [n one many] (str n " " (if (= n 1) one many)))

(def dict
  {:fmt
   {:note note
    :tonic tonic
    :pc pc
    :signature (fn [_ s] (cond (zero? s) "χωρίς αλλοιώσεις" (pos? s) (str s " ♯") :else (str (- s) " ♭")))
    :note-summary (fn [_ {:keys [note-octaves note-accidentals note-reference]}]
                    (str/join " · " [(plural note-octaves "οκτάβα" "οκτάβες")
                                     (if note-accidentals "και οι 12 φθόγγοι" "λευκά πλήκτρα")
                                     (if (= note-reference :none) "χωρίς αναφορά" "με αναφορά")]))
    :dict-summary (fn [loc {:keys [key beats bars chromatic repeats]}]
                    (str/join " · " (cond-> [(key-name loc key) (str beats "/4") (plural bars "μέτρο" "μέτρα")]
                                      chromatic (conj "χρωματικό")
                                      true (conj (if (= repeats :free) "ελεύθερο" "αυστηρό")))))
    :wrong-note (fn [_ target answer] [:<> "Ήταν " [:b target] ", έπαιξες " [:b answer]])
    :result (fn [_ ok enh n] (str ok (when (pos? enh) (str " + " enh " με άλλη γραφή")) " από " n " φθόγγους σωστοί"))
    :bar-of (fn [_ b n] (str "Μέτρο " b " από " n))
    :hearing (fn [_ b] (str " · ακούγονται τα μέτρα " (dec b) "–" b))
    :plays-left (fn [_ n] (str "απομένουν " n (if (= n 1) " ακρόαση" " ακροάσεις")))
    :bar-wrong (fn [_ n] (plural n "λάθος" "λάθη"))
    :attempts (fn [_ n] (plural n "απάντηση" "απαντήσεις"))}

   :nav {:note "Φθόγγος" :dictation "Ντικτέ" :stats "Στατιστικά" :settings "Ρυθμίσεις"}

   :ui
   {:loading-sounds "φόρτωση ήχων…"
    :samples-failed "Δεν φόρτωσαν οι ήχοι του πιάνου."
    :language "Γλώσσα"
    :note-names "Ονόματα φθόγγων"
    :solfege-sample "Ντο Ρε Μι"
    :clef "Κλειδί"
    :treble "Σολ" :bass "Φα" :grand "Διπλό πεντάγραμμο"
    :range "Έκταση"
    :narrow "Στενή" :wide "Ευρεία"
    :midi "Πληκτρολόγιο MIDI"
    :midi-hint "Απάντηση παίζοντας σε συνδεδεμένο πληκτρολόγιο MIDI."
    :midi-none "Δεν βρέθηκαν είσοδοι MIDI."
    :midi-unsupported "Αυτός ο browser δεν υποστηρίζει Web MIDI (Safari)."
    :midi-denied "Δεν δόθηκε πρόσβαση στο MIDI."
    :options "Επιλογές"
    :octaves "Οκτάβες"
    :one-octave "1 οκτάβα" :three-octaves "3 οκτάβες"
    :reference "Φθόγγος αναφοράς"
    :none "Κανένας"
    :reference-hint "Με φθόγγο αναφοράς, ακούγεται πριν από τον πρώτο φθόγγο και μένει σε κουμπί. Χωρίς αυτόν είναι εξάσκηση απόλυτης ακοής."
    :accidentals "Αλλοιώσεις"
    :accidentals-hint "Και οι 12 φθόγγοι, όχι μόνο τα λευκά πλήκτρα."
    :auto-next "Επόμενος φθόγγος αμέσως μετά από σωστή απάντηση"
    :note-intro "Ακούγεται ένας φθόγγος: βρες τον στο πληκτρολόγιο."
    :which-note "Ποιος φθόγγος ήταν;"
    :correct "Σωστά:"
    :start "Έναρξη"
    :replay "🔊 Ξανά"
    :reference-btn (fn [_ name] (str "Αναφορά " name))
    :hear-yours "Άκου τον δικό σου"
    :next "Επόμενος"
    :note-keys [:<> [:kbd "Enter"] " έναρξη / επόμενος, " [:kbd "Space"] " ξανά, " [:kbd "R"] " αναφορά."]
    :key "Τονικότητα"
    :major "μείζονα" :minor "ελάσσονα"
    :meter "Μέτρο"
    :bars "Μέτρα"
    :repeats "Ακροάσεις"
    :strict "Αυστηρό (2 φορές)" :free "Ελεύθερο"
    :chromatic "Χρωματικοί φθόγγοι"
    :chromatic-hint "Φθόγγοι εκτός κλίμακας, που οδηγούν με ημιτόνιο: με δίεση προς τα πάνω, με ύφεση προς τα κάτω."
    :first-given "Δίνεται ο πρώτος φθόγγος"
    :start-dictation "Έναρξη ντικτέ"
    :listen-scale "Άκου: η κλίμακα, ανεβαίνοντας και κατεβαίνοντας."
    :skip "Παράλειψη"
    :play-step "▶ Άκου"
    :hear-bar "▶ Αυτό το μέτρο"
    :hear-scale "Κλίμακα"
    :check "Έλεγχος"
    :next-bar "Επόμενο μέτρο"
    :bar-correct "Όλα σωστά"
    :hear-melody "▶ Μελωδία"
    :hear-answer "▶ Η απάντησή σου"
    :new-dictation "Νέο ντικτέ"
    :quit "Διακοπή"
    :settings-back "Ρυθμίσεις"
    :tool-up "Ένα βήμα πάνω (↑)"
    :tool-down "Ένα βήμα κάτω (↓)"
    :tool-sharp "Δίεση (#)"
    :tool-flat "Ύφεση (-)"
    :tool-natural "Αναίρεση (n)"
    :tool-respell "Εναρμόνια γραφή (x)"
    :tool-delete "Διαγραφή (⌫)"
    :dict-keys [:<> "Γράψε κάθε μέτρο αφού το ακούσεις μαζί με το προηγούμενο. Πάτησε στο πεντάγραμμο, παίξε στο πληκτρολόγιο ή γράψε "
                [:kbd "A"] "–" [:kbd "G"] " (C = Ντο … B = Σι)· " [:kbd "↑"] [:kbd "↓"] " μετακίνηση, " [:kbd "#"] " " [:kbd "-"] " " [:kbd "n"]
                " αλλοιώσεις, " [:kbd "x"] " εναρμόνια, " [:kbd "⌫"] " διαγραφή, " [:kbd "Space"] " άκου, "
                [:kbd "Enter"] " έλεγχος / επόμενο."]
    :no-attempts "Δεν υπάρχουν απαντήσεις ακόμα."
    :no-dictations "Δεν έχει ολοκληρωθεί κανένα ντικτέ ακόμα."
    :last-20 "τελευταίες 20"
    :by-note "Ανά φθόγγο"
    :by-octave "Ανά οκτάβα"
    :confusions "Συχνότερες συγχύσεις"
    :clear-stats "Διαγραφή στατιστικών"
    :sure "Οριστική διαγραφή;"
    :clear "Διαγραφή"
    :cancel "Άκυρο"}})
