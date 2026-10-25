(ns dicteeineer.stats)

(def recent-window 20)

(defn summarize [xs]
  (let [n (count xs)]
    {:n n :acc (when (pos? n) (/ (count (filter :correct xs)) n))}))

(defn- rows [kfn xs]
  (->> (group-by kfn xs)
       (map (fn [[k xs]] (assoc (summarize xs) :key k)))
       (sort-by :key)))

(defn note-stats
  "Pitch classes are reported by their sharp spelling's MIDI note in octave 4."
  [attempts]
  {:total (summarize attempts)
   :recent (summarize (take-last recent-window attempts))
   :by-pc (rows #(mod (:target %) 12) attempts)
   :by-octave (rows #(dec (quot (:target %) 12)) attempts)
   :confusions (->> (remove :correct attempts)
                    (group-by (juxt :target :answer))
                    (map (fn [[[t a] xs]] {:target t :answer a :n (count xs)}))
                    (sort-by :n >)
                    (take 8))})
