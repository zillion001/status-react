(ns status-im.utils.datetime
  (:require [cljs-time.core :as cljs-time]
            [cljs-time.coerce :as coerce]
            [cljs-time.format :as format]
            [cljs-time.periodic :as periodic]
            [status-im.i18n :as i18n]
            goog.string.format))

(defn now []
  (cljs-time/now))

(def hour (* 1000 60 60))
(def day (* hour 24))
(def week (* 7 day))
(def units [{:name :t/datetime-second :limit 60 :in-second 1}
            {:name :t/datetime-minute :limit 3600 :in-second 60}
            {:name :t/datetime-hour :limit 86400 :in-second 3600}
            {:name :t/datetime-day :limit nil :in-second 86400}])

(def time-zone-offset (cljs-time/hours (- (/ (.getTimezoneOffset (js/Date.)) 60))))

(defn to-short-str
  ([ms]
   (to-short-str ms #(format/unparse (format/formatters :hour-minute) %)))
  ([ms today-format-fn]
   (let [date      (coerce/from-long ms)
         local     (cljs-time/plus date time-zone-offset)
         today     (cljs-time/today-at-midnight)
         yesterday (cljs-time/plus today (cljs-time/days -1))]
     (cond
       (cljs-time/before? date yesterday) (format/unparse (format/formatter "dd MMM hh:mm") local)
       (cljs-time/before? date today) (i18n/label :t/datetime-yesterday)
       :else (today-format-fn local)))))

(defn timestamp->mini-date [ms]
  (format/unparse (format/formatter "dd MMM") (-> ms
                                                  coerce/from-long
                                                  (cljs-time/plus time-zone-offset))))

(defn timestamp->time [ms]
  (format/unparse (format/formatter "HH:mm") (-> ms
                                                 coerce/from-long
                                                 (cljs-time/plus time-zone-offset))))

(defn timestamp->date-key [ms]
  (keyword (format/unparse (format/formatter "YYYYMMDD") (-> ms
                                                             coerce/from-long
                                                             (cljs-time/plus time-zone-offset)))))

(defn timestamp->long-date [ms]
  (keyword (format/unparse (format/formatter "MMM DD YYYY HH:mm:ss")
                           (-> ms
                               coerce/from-long
                               (cljs-time/plus time-zone-offset)))))

(defn day-relative [ms]
  (when (pos? ms)
    (to-short-str ms #(i18n/label :t/datetime-today))))

(defn format-time-ago [diff unit]
  (let [name (i18n/label-pluralize diff (:name unit))]
    (i18n/label :t/datetime-ago-format {:ago            (i18n/label :t/datetime-ago)
                                        :number         diff
                                        :time-intervals name})))

(defn time-ago [time]
  (let [diff (cljs-time/in-seconds (cljs-time/interval time (cljs-time/now)))]
    (if (< diff 60)
      (i18n/label :t/active-online)
      (let [unit (first (drop-while #(and (>= diff (:limit %))
                                          (:limit %))
                                    units))]
        (-> (/ diff (:in-second unit))
            Math/floor
            int
            (format-time-ago unit))))))

(defn to-date [ms]
  (coerce/from-long ms))

(defn now-ms []
  (coerce/to-long (now)))

(defn format-date [format date]
  (let [local (cljs-time/plus (coerce/from-date date) time-zone-offset)]
    (format/unparse (format/formatter format) local)))

(defn get-ordinal-date [date]
  (let [local (cljs-time/plus (coerce/from-date date) time-zone-offset)
        day   (js/parseInt (format/unparse (format/formatter "d") local))
        s     {0 "th"
               1 "st"
               2 "nd"
               3 "rd"}
        m     (mod day 100)]
    (str day (or (s (mod (- m 20) 10))
                 (s m)
                 (s 0)))))

(defn last-n-days [n step]
  (let [end-date   (cljs-time/today-at-midnight)
        start-date (cljs-time/minus end-date (cljs-time/days n))]
    (periodic/periodic-seq start-date end-date (cljs-time/days step))))
