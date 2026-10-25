(ns dicteeiner.audio)

(def sample-notes (vec (range 36 97 3)))

(defonce ctx (atom nil))
(defonce out (atom nil))
(defonce buffers (atom {}))
(defonce voices (atom []))

(defn- context []
  (or @ctx
      (let [Ctor (or (.-AudioContext js/window) (.-webkitAudioContext js/window))
            c (Ctor.)
            comp (.createDynamicsCompressor c)
            master (.createGain c)]
        (set! (.. master -gain -value) 0.9)
        (.connect master comp)
        (.connect comp (.-destination c))
        (reset! out master)
        (reset! ctx c))))

(defn install-unlock!
  "iOS only resumes audio inside a user-gesture call stack, so do it synchronously."
  []
  (when-let [session (.-audioSession js/navigator)]
    (set! (.-type session) "playback"))
  (let [unlock (fn []
                 (let [c (context)]
                   (when (not= "running" (.-state c))
                     (.resume c)
                     (let [src (.createBufferSource c)]
                       (set! (.-buffer src) (.createBuffer c 1 1 22050))
                       (.connect src (.-destination c))
                       (.start src 0)))))]
    (doseq [ev ["pointerdown" "touchend" "keydown"]]
      (.addEventListener js/document ev unlock #js {:capture true}))))

(defn load!
  "Fetches and decodes all samples; calls `on-done` with the number loaded."
  [on-done]
  (let [c (context)]
    (-> (js/Promise.all
         (clj->js
          (for [n sample-notes]
            (-> (js/fetch (str "samples/" n ".mp3"))
                (.then #(.arrayBuffer %))
                (.then #(js/Promise. (fn [ok fail] (.decodeAudioData c % ok fail))))
                (.then #(swap! buffers assoc n %))
                (.catch #(js/console.warn "sample failed" n %))))))
        (.then #(on-done (count @buffers))))))

(defn- nearest-sample [midi]
  (apply min-key #(Math/abs (- % midi)) (keys @buffers)))

(defn- play-note! [midi start dur gain]
  (when (seq @buffers)
    (let [c (context)
          s (nearest-sample midi)
          src (.createBufferSource c)
          g (.createGain c)
          end (+ start dur)]
      (set! (.-buffer src) (@buffers s))
      (set! (.. src -playbackRate -value) (Math/pow 2 (/ (- midi s) 12)))
      (.setValueAtTime (.-gain g) gain start)
      (.setValueAtTime (.-gain g) gain end)
      (.exponentialRampToValueAtTime (.-gain g) 0.0001 (+ end 0.6))
      (.connect src g)
      (.connect g @out)
      (.start src start)
      (.stop src (+ end 0.7))
      (swap! voices conj [src g]))))

(defn stop-all! []
  (when-let [c @ctx]
    (let [now (.-currentTime c)]
      (doseq [[src g] @voices]
        (try
          (.cancelScheduledValues (.-gain g) now)
          (.setTargetAtTime (.-gain g) 0 now 0.03)
          (.stop src (+ now 0.2))
          (catch :default _)))))
  (reset! voices []))

(defn play-events!
  "`voices` is how many notes sound together; defaults to the distinct pitches in `events`."
  [events & [voices]]
  (let [c (context)
        t0 (+ (.-currentTime c) 0.05)
        gain (/ 0.6 (Math/sqrt (max 1 (or voices (count (distinct (map first events)))))))]
    (doseq [[m off dur] events]
      (play-note! m (+ t0 off) dur gain))))

(defn play-single! [midi dur]
  (play-events! [[midi 0 dur]]))
