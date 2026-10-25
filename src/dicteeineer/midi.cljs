(ns dicteeineer.midi
  (:require [re-frame.core :as rf]))

(defonce access (atom nil))

(defn- on-message [e]
  (let [[status note vel] (array-seq (.-data e))
        cmd (bit-and status 0xf0)]
    (cond
      (and (= cmd 0x90) (pos? vel)) (rf/dispatch [:midi/note-on note])
      (or (= cmd 0x80) (= cmd 0x90)) (rf/dispatch [:midi/note-off note]))))

(defn- attach! [a]
  (let [names (atom [])]
    (.forEach (.-inputs a)
              (fn [input]
                (set! (.-onmidimessage input) on-message)
                (swap! names conj (.-name input))))
    (rf/dispatch [:midi/status {:state :ready :inputs @names}])))

(defn connect! []
  (cond
    @access (attach! @access)
    (not (.-requestMIDIAccess js/navigator))
    (rf/dispatch [:midi/status {:state :unsupported :inputs []}])
    :else
    (-> (.requestMIDIAccess js/navigator)
        (.then (fn [a]
                 (reset! access a)
                 (set! (.-onstatechange a) #(attach! a))
                 (attach! a)))
        (.catch #(rf/dispatch [:midi/status {:state :denied :inputs []}])))))

(defn disconnect! []
  (when-let [a @access]
    (.forEach (.-inputs a) #(set! (.-onmidimessage %) nil))
    (set! (.-onstatechange a) nil)
    (reset! access nil)))
