(ns dicteeineer.storage
  (:require ["idb-keyval" :as idb]
            [cljs.reader :as reader]))

;; Each app keeps its own database. On music-trainer.app the apps share one origin, and in
;; idb-keyval's default store they used to overwrite each other's keys (settings, attempts…).

(def store (delay (idb/createStore "dicteeineer" "keyval")))

(def ^:private ours
  "How to recognize this app's data among the keys of the old shared store."
  {:settings #(contains? % :dict-key)
   :attempts #(every? (fn [a] (contains? a :target)) %)
   :history #(every? (fn [h] (contains? h :beats)) %)
   :dict #(contains? % :melody)})

(defn- ours? [k v] (try (boolean ((get ours k (constantly false)) v)) (catch :default _ false)))

(defn- parse [s] (when (string? s) (try (reader/read-string s) (catch :default _ nil))))

(defn save! [k v]
  (-> (idb/set (name k) (pr-str v) @store)
      (.catch #(js/console.warn "save failed" (name k) %))))

(defn load-all!
  "Loads `ks` from this app's store. The first time, keys it doesn't have yet are copied from
   the old shared store when the value there is recognizably this app's."
  [ks on-done]
  (-> (js/Promise.all (clj->js (for [k (conj ks :migrated)] (idb/get (name k) @store))))
      (.then (fn [vals]
               (let [own (zipmap ks (map parse vals))
                     missing (if (last vals) [] (filterv #(nil? (own %)) ks))]
                 (-> (js/Promise.all (clj->js (for [k missing] (idb/get (name k)))))
                     (.then (fn [old]
                              (let [found (into {} (for [[k v] (map vector missing (map parse old))
                                                         :when (and (some? v) (ours? k v))]
                                                     [k v]))]
                                (doseq [[k v] (assoc found :migrated true)] (save! k v))
                                (on-done (merge own found)))))))))
      (.catch (fn [e]
                (js/console.warn "load failed" e)
                (on-done {})))))

(defn clear! [] (-> (idb/clear @store) (.then #(save! :migrated true))))
