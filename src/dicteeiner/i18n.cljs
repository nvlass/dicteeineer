(ns dicteeiner.i18n
  (:require [clojure.string :as str]
            [re-frame.core :as rf]
            [tongue.core :as tongue]
            [dicteeiner.theory :as t]
            [dicteeiner.i18n.en :as en]
            [dicteeiner.i18n.el :as el]))

;; Every entry is a string, a hiccup vector, or a fn of (loc & args). `loc` is {:lang :names},
;; :names being the seven note names in use, so the dictionaries can name notes without re-frame.

;; Hiccup values pass through as they are; without this tongue would call vectors as fns.
(extend-type PersistentVector
  tongue/IInterpolate
  (interpolate-named [v _ _ _] v)
  (interpolate-positional [v _ _ _] v))

(def solfege
  {:en ["Do" "Re" "Mi" "Fa" "Sol" "La" "Si"]
   :el ["Ντο" "Ρε" "Μι" "Φα" "Σολ" "Λα" "Σι"]})

(def translate (tongue/build-translate {:en en/dict :el el/dict :tongue/fallback :en}))

(defn locale [lang note-names]
  {:lang lang :names (if (= note-names :solfege) (solfege lang) t/letters)})

(defn browser-lang []
  (let [l (some-> js/globalThis .-navigator .-language)]
    (if (and l (str/starts-with? l "el")) :el :en)))

(defn t [loc k & args] (apply translate (:lang loc) k loc args))

(defn tr [k & args] (apply t @(rf/subscribe [:loc]) k args))
