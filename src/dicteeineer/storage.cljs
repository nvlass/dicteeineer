(ns dicteeineer.storage
  (:require ["idb-keyval" :as idb]
            [cljs.reader :as reader]))

(defn save! [k v]
  (-> (idb/set (name k) (pr-str v))
      (.catch #(js/console.warn "save failed" (name k) %))))

(defn load-all! [ks on-done]
  (-> (js/Promise.all (clj->js (map #(idb/get (name %)) ks)))
      (.then (fn [vals]
               (on-done (zipmap ks (map #(when (string? %) (reader/read-string %)) vals)))))
      (.catch (fn [e]
                (js/console.warn "load failed" e)
                (on-done {})))))
