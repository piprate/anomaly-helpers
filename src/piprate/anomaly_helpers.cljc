(ns piprate.anomaly-helpers
  (:require
   [clojure.spec.alpha :as s]
   [cognitect.anomalies :as anom])

  ;; self require macros for cljs builds
  #?(:cljs
     (:require-macros [piprate.anomaly-helpers])))

(def retryable-categories #{::anom/unavailable ::anom/interrupted ::anom/busy})

(defn with-message [a msg]
  (if msg
    (assoc a ::anom/message msg)
    a))

(defn augment
  ([m cat]
   (assoc m ::anom/category cat))
  ([m cat msg]
   (-> m
       (augment cat)
       (with-message msg))))

(defn anomaly
  ([cat]
   {::anom/category cat})
  ([cat message]
   (with-message (anomaly cat) message))
  ([cat message extra-data]
   (augment extra-data cat message)))

(defn anomaly? [a]
  (s/valid? ::anom/anomaly a))

(defn retryable? [a]
  (retryable-categories (::anom/category a)))

(defn message [a]
  (::anom/message a))

(defn category [a]
  (::anom/category a))


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


(defn ex->anomaly [ex]
  #?(:clj
     (augment {::exception ex}
              ::anom/fault
              (.getMessage ex))

     :cljs
     (augment {::exception ex}
              ::anom/fault
              (.-message ex))))


(defprotocol ToAnomaly
  :extend-via-metadata true
  (-to-anomaly [o]
    "Coerces o to an anomaly. If o is not anomalous returns o as is."))

#?(:clj
   (extend-protocol ToAnomaly
     Object
     (-to-anomaly [o] o)

     nil
     (-to-anomaly [_] nil)

     Throwable
     (-to-anomaly [ex] (ex->anomaly ex)))

   :cljs
   (extend-protocol ToAnomaly
     default
     (-to-anomaly [o] o)

     js/Error
     (-to-anomaly [ex] (ex->anomaly ex))))


(defn to-anomaly [o]
  (if (anomaly? o)
    o
    (-to-anomaly o)))
