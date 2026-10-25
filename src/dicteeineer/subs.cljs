(ns dicteeineer.subs
  (:require [re-frame.core :as rf]
            [dicteeineer.stats :as stats]
            [dicteeineer.i18n :as i18n]))

(doseq [k [:view :settings :note :dict :attempts :history :samples :midi :playing :width]]
  (rf/reg-sub k (fn [db _] (k db))))

(rf/reg-sub :loc :<- [:settings] (fn [s _] (i18n/locale (:lang s) (:note-names s))))

(rf/reg-sub :note-stats :<- [:attempts] (fn [xs _] (stats/note-stats xs)))
