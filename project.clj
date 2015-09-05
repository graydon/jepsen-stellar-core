(defproject jepsen-stellar-core "0.1.0-SNAPSHOT"
  :description "An experiment in testing stellar-core under jepsen"
  :url "http://stellar.org"
  :license {:name "Apache License 2.0"
            :url "http://www.apache.org/licenses/LICENSE-2.0"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.clojure/core.incubator "0.1.3"]
                 [jepsen "0.0.6"]
                 [clj-http "2.0.0"]
                 [cheshire "5.5.0"]])
