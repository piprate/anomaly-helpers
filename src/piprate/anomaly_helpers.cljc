(ns piprate.anomaly-helpers
  (:require
   [clojure.spec.alpha :as s]
   [cognitect.anomalies :as anom])

  ;; self require macros for cljs builds
  #?(:cljs
     (:require-macros [piprate.anomaly-helpers])))

(def retryable-categories #{::anom/unavailable ::anom/interrupted ::anom/busy})

(defn anomaly
  ([cat]
   {::anom/category cat})
  ([cat message]
   {::anom/category cat
    ::anom/message message})
  ([cat message extra-data]
   (merge
    extra-data
    {::anom/category cat
     ::anom/message message})))

(defn augment [m cat]
  (assoc m ::anom/category cat))

(defn anomaly? [a]
  (s/valid? ::anom/anomaly a))

(defn retryable? [a]
  (retryable-categories (::anom/category a)))

(defn message [a]
  (::anom/message a))

(defn category [a]
  (::anom/category a))

(defn with-message [a msg]
  (assoc a ::anom/message msg))

(defmacro alet [bindings & body]
  (if (seq bindings)
    (let [name           (first bindings)
          expression     (second bindings)
          other-bindings (next (next bindings))]
      `(let [tmp# ~expression]
         (if (anomaly? tmp#)
           tmp#
           (let [~name tmp#]
             (alet ~other-bindings ~@body)))))
    `(do
       ~@body)))

(defmacro a->
  "like -> but stops on the first anomaly encounted while threading"
  [value & fs]
  (if (seq fs)
    (let [first-call (first fs)
          other-calls (next fs)]
      `(let [tmp# ~value]
         (if (anomaly? tmp#)
           tmp#
           (a-> (-> tmp# ~first-call) ~@other-calls))))
    value))

(defmacro a->>
  "like ->> but stops on the first anomaly encounted while threading"
  [value & fs]
  (if (seq fs)
    (let [first-call (first fs)
          other-calls (next fs)]
      `(let [tmp# ~value]
         (if (anomaly? tmp#)
           tmp#
           (a->> (->> tmp# ~first-call) ~@other-calls))))
    value))
