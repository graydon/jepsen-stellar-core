(ns jepsen-stellar-core.core-test
  (:require [clojure.test :refer :all]
            [clojure.pprint :refer [pprint]]
            [jepsen-stellar-core.core :refer :all]
            [jepsen [core :as jepsen]
             [report :as report]]))

(defn run-set-test!
  "Runs a test around set creation and dumps some results to the report/ dir"
  [t]
  (let [test (jepsen/run! t)]
    (or (is (:valid? (:results test)))
        (println (:error (:results test))))
    (report/to (str "report/" (:name test) "/history.edn")
               (pprint (:history test)))
    (report/to (str "report/" (:name test) "/set.edn")
               (pprint (:set (:results test))))))

(deftest hammer
  (run-set-test! (hammer-test)))

(deftest bridge
  (run-set-test! (bridge-test)))

(deftest majorities-ring
  (run-set-test! (majorities-ring-test)))

(deftest random-split
  (run-set-test! (random-split-test)))

(deftest random-isolate
  (run-set-test! (random-isolate-test)))

(deftest flaky-net
  (run-set-test! (flaky-net-test)))

(deftest slow-net
  (run-set-test! (slow-net-test)))

