(ns piprate.anomaly-helpers-test
  (:require
   [clojure.test :refer [deftest testing is]]
   [cognitect.anomalies :as anom]
   [piprate.anomaly-helpers :as ah]))


(def non-anoms ["a" :a {:foo :bar} {} [1] [] '() nil false true])

(defn sample-anom []
  {::anom/category ::anom/unsupported
   ::anom/message "fail"})

(defn sample-fn []
  :ok)


(deftest anomaly-test
  (is (= {::anom/category ::anom/busy} (ah/anomaly ::anom/busy)))
  (is (= {::anom/category ::anom/busy
          ::anom/message "go away"}
         (ah/anomaly ::anom/busy "go away")))

  (is (= {::anom/category ::anom/busy
          ::anom/message "go away"
          :foo :bar}
         (ah/anomaly ::anom/busy "go away" {:foo :bar})))

  (testing "Category and message in extra-data are overwritten"
    (is (= {::anom/category ::anom/busy
            ::anom/message "go away"
            :foo :bar}
           (ah/anomaly ::anom/busy
                       "go away"
                       {:foo :bar
                        ::anom/category ::anom/unsupported
                        ::anom/message "old message"})))))

(deftest anomaly?-test
  (doseq [non-anom non-anoms]
    (is (= false (ah/anomaly? non-anom))))

  (doseq [anom [{::anom/category ::anom/busy}
                {::anom/category ::anom/unsupported ::anom/message "fail"}
                {::anom/category ::anom/unsupported :foo :bar}]]
    (is (= true (ah/anomaly? anom)))))

(deftest message-test
  (testing "anomaly with message"
    (is (= "fail" (ah/message (sample-anom)))))

  (testing "non anomalies"
    (doseq [non-anom non-anoms]
      (is (nil? (ah/message non-anom))))))


(deftest alet-test
  (is (= :ok (ah/alet [x (sample-fn)] x)))

  (is (= [1 2] (ah/alet [x (identity 1)
                         y (identity 2)]
                 [x y])))


  (is (= (sample-anom) (ah/alet [a (sample-anom)
                                 x (sample-fn)]
                         x)))

  (is (= :ok (ah/alet [] :ok))
      "with no bindings"))


(deftest a->-test
  (let [x 1
        a (sample-anom)
        failing-f (fn [& args]
                    a)]
    (is (= 1 (ah/a-> x)))
    (is (= 2 (ah/a-> x inc)))
    (is (= -8 (ah/a-> x inc (- 10))))

    (is (= a (ah/a-> a)))
    (is (= a (ah/a-> x inc failing-f dec)))))


(deftest a->>-test
  (let [x 1
        a (sample-anom)
        failing-f (fn [& args]
                    a)]
    (is (= 1 (ah/a->> x)))
    (is (= 2 (ah/a->> x inc)))
    (is (= 8 (ah/a->> x inc (- 10))))

    (is (= a (ah/a->> a)))
    (is (= a (ah/a->> x inc failing-f dec)))))
