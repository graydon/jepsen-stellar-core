(ns jepsen-stellar-core.core
  (:use [clojure.core.strint])
  (:require [clj-http.client :as http]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure.tools.logging :refer [warn info debug]]
            [jepsen.os.debian :as debian]
            [jepsen [client :as client]
             [core :as jepsen]
             [db :as db]
             [tests :as tests]
             [control :as c :refer [|]]
             [checker :as checker]
             [nemesis :as nemesis]
             [model :as model]
             [generator :as gen]
             [util :refer [timeout meh log-op]]]))

;; Bunch of random node keypairs ("identities")

(def nodes
  {:n1 {:sec "SCECGNDJ6ZKTLHC2EBQSQ7LWATWIRBZCIWUHTD23SR3QCB4JQEY4DMJL"
        :pub "GBMEX3FCTKJVM25IXTRJZKSD6Z3HJXAJ6XGZN5F3WAO4UIN6RKXL3EIC"}
   :n2 {:sec "SAS66K2FZKXYCKXQIPV5H6SNK6WG3FRSBVEBXY4KOCSTHLZLT6E2XJBW"
        :pub "GCB6H5QKFTUISFST4CDT2XFERDQV4TZPT3BQCMT53NBGVTQNLXYBYYMP"}
   :n3 {:sec "SDLBP3HWZBHMJ26JNKN55DALEHZJQVT2BSNB2I6KISZYHY6KOGPYLJSK"
        :pub "GCS5GQQCVOWOILCD7QBUGYMAALK372MXFGACHFA2G2YXPOTC6IHYVYOP"}
   :n4 {:sec "SBNJ7KIFJLNOFRWXUYVLG2CAEORXE3XK3WQXSWXOKXERZ3B2W6HRNH7P"
        :pub "GDQ4LOJEICOCFW34LK4WYIUM5UPNA6ONI43QSZ5MLEFZDYOVL5IISQH3"}
   :n5 {:sec "SBTQU46R47J2CDU65E32AITEN6XE7QETQLILAB2LGG3AZ6AE4ZWVBSMB"
        :pub "GB3WGEHMELIWXSYCNOQ3OZ4CTSKGSNDYIMZYVG6OHNDYABLESPZI7CJQ"}})

(def nodenames (sort (keys nodes)))

(defn nodenum [n]
  (nth nodenames (rem n (count nodenames))))

(def ^:dynamic *stellar-core-version*
  "0.0.1-132-4654e395")

(def stellar-core-deb-url-path
  "https://s3.amazonaws.com/stellar.org/releases/stellar-core")

(defn stellar-core-deb [version]
  (<< "stellar-core-~{version}_amd64.deb"))

(defn stellar-core-deb-url [version]
  (<< "~{stellar-core-deb-url-path}/~(stellar-core-deb version)"))



;; Basic mechanisms for talking to a stellar-core server's test port

(def ten-million 10000000)
(def ^:dynamic *server-host* "localhost")
(def ^:dynamic *server-port* 11626)

(defn get-json [path & [qp]]
  (let [params {:as :json}]
    (http/get (<< "http://~{*server-host*}:~{*server-port*}/~{path}")
              (if qp
                (assoc params :query-params qp)
                params))))


(defn server-info []
  (-> (get-json "info") :body :info))

(defn ledger-num []
  (-> (server-info) :ledger :num))

(defn server-status []
  (:state (server-info)))

(defn server-metrics []
  (-> (get-json "metrics") :body :metrics))

(defn test-tx [qp]
  (-> (get-json "testtx" qp) :body))

(defn get-account [who]
  (-> (get-json "testacc" {:name who}) :body))

(defn has-account [who]
  (-> (get-account who) (contains? :seqnum)))

(defn payment-qp [from to amount]
  {:from from
   :to to
   :amount amount})

(defn create-account-qp [to]
  (assoc (payment-qp "root" to (* 10000 ten-million))
         :create 'true))

(defn create-account-from-qp [from to]
  (assoc (payment-qp from to (* 100 ten-million))
         :create 'true))

(defn do-payment [from to amount]
  (test-tx (payment-qp from to amount)))

(defn do-create-account-from [from to]
  (test-tx (create-account-from-qp from to)))

(defn do-create-account [to]
  (test-tx (create-account-qp to)))

(defn retry-until [& {:keys [ledgers
                             retries
                             f
                             until]
                      :or {ledgers 100
                           retries 100
                           f #()
                           until #(or false)}}]
  (let [first (ledger-num)
        last (+ first ledgers)]
    (loop [retried 0]
      (let [curr (ledger-num)]
        (f)
        (cond
          (until) :ok
          (> curr last) :fail
          (> retried retries) :fail
          (> retried 20)
          (do
            (Thread/sleep 800)
            (info (<< "awaiting close: node ~{*server-host*}, ledger ~{curr}, status ~(server-status)"))
            (recur (inc retried)))
          true
          (do
            (Thread/sleep 300)
            (recur (inc retried))))))))

(defn install!
  [node version]
  (when-not (or
             (debian/installed? :libpq5)
             (debian/installed? :libpq5:amd64))
    (debian/update!)
    (debian/install '(:libpq5:amd64)))
  (when-not (or
             (debian/installed? :libsqlite3-0)
             (debian/installed? :libsqlite3-0:amd64))
    (debian/update!)
    (debian/install '(:libsqlite3-0:amd64)))
  (when-not (and
             (or
              ; Different dpkg versions report this slightly differently
              (debian/installed? :stellar-core:amd64)
              (debian/installed? :stellar-core))
             (= (debian/installed-version :stellar-core) version))
    (c/exec :wget :--no-clobber (stellar-core-deb-url version))
    (meh (debian/uninstall! :stellar-core))
    (c/exec :dpkg :-i (stellar-core-deb version))))

(defn configure!
  [node]
  (let [self (nodes node)
        others (vec (sort (filter #(not= %1 node) (keys nodes))))]

    (c/upload '("/root/.ssh/known_hosts"
                "/root/.ssh/id_rsa"
                "/root/.ssh/id_rsa.pub")
              "/root/.ssh" :mode 0600)
    (c/exec :echo
            (slurp (io/resource "stellar-core"))
            :> "/etc/init.d/stellar-core")
    (c/exec :chmod :0755 "/etc/init.d/stellar-core")
    (c/exec :echo
            (-> (io/resource "stellar-core.cfg")
                slurp
                (str/replace #"%VALIDATION_SEED%" (:sec self))
                (str/replace #"%PUBKEY(\d)%"
                             (fn [[_ n]] ((nodes (keyword (str "n" n))) :pub)))
                (str/replace #"%SELF%" (name node))
                (str/replace #"%OTHER(\d)%"
                             (fn [[_ n]] (name (others (- (read-string n) 1))))))
            :> "stellar-core.cfg")))

(defn wipe!
  []
  (meh (c/exec :service :stellar-core :stop))
  (c/exec :rm :-f
          "/etc/init.d/stellar-core"
          "/root/.ssh/known_hosts"
          "/root/.ssh/id_rsa"
          "/root/.ssh/id_rsa.pub")
  (c/exec :rm :-rf :history :buckets :stellar.db :stellar-core.cfg :stellar-core.log))

(defn initialize!
  [node]
  (c/exec :stellar-core :--conf :stellar-core.cfg :--newhist node)
  (c/exec :stellar-core :--conf :stellar-core.cfg :--newdb)
  (c/exec :stellar-core :--conf :stellar-core.cfg :--forcescp))

;; Configuration loading

;; For jepsen to manage a server, we must reify the db/DB protocol
(defn db
  [version]
  (reify db/DB
    (setup! [db test node]
      (wipe!)
      (install! node version)
      (configure! node)
      (initialize! node)
      (c/exec :service :stellar-core :start))

    (teardown! [db test node]
      (meh (c/exec :service :stellar-core :stop)))
    ))

;; We dynamically track the max-account to read-back during any test.
;; This number only ever increases; it's harmless if it's "too high",
;; we just do a bunch of unsuccessful queries to nonexistent high values
;; at the end of a test.
(def max-account (atom 0))
(defn account-id [n] (keyword (<< "account~{n}")))

;; For jepsen to apply operations, we must reify client/Client
(defn client
  [node]
  (reify client/Client
    (setup!    [this test node] (client node))
    (teardown! [this test])
    (invoke!   [this test op]
      (case (:f op)

        :setup
        (binding [*server-host* (name node)]
          (let [n (nth nodenames (:value op))]
            (assoc op :type (retry-until
                             :f #(do-create-account n)
                             :until #(has-account n)))))

        :read
        (binding [*server-host* (name node)]
          (assoc op
                 :type :ok,
                 :value (apply sorted-set
                               (filter (fn [n] (has-account (account-id n)))
                                       (range (+ 1 @max-account))))))

        :add
        (binding [*server-host* (name node)]
          (let [v (:value op)
                id (account-id v)
                src (nodenum v)]
            (assoc op :type (retry-until
                             :f #(do-create-account-from src id)
                             :until #(has-account id)))))
        ))))

(defn setup
  "Generator that invokes :setup on every node, once, single-threaded"
  []
  (gen/singlethreaded
   (apply gen/concat
          (map (fn [node] (gen/on (fn [process] (= process node))
                               (gen/once {:type :invoke :f :setup :value node})))
               (range (count nodenames))))))

(defn adds
  "Generator that emits :add operations for sequential integers."
  []
  (->> (range)
       (map (fn [x]
              (swap! max-account (fn [e] (max e x)))
              {:type :invoke, :f :add, :value x}))
       gen/seq))

(defn recover
  "A generator which stops the nemesis and allows some time for recovery."
  []
  (gen/nemesis
    (gen/phases
      (gen/once {:type :info, :f :stop})
      (gen/sleep 20))))

(defn read-once
  "A generator which reads exactly once."
  []
  (gen/clients
    (gen/once {:type :invoke, :f :read})))


;; All our tests follow a simple phase structure: they sync up a new network,
;; run for 10 minutes ':add'ing sequential accounts and running nemesis disruptions
;; for 20 seconds every minute (with 40s to recover from each). The network
;; is then allowed to "fully heal" and a final :read is performed.
(defn simple-test
  [version]
  (assoc tests/noop-test
         :name "stellar-core"
         :os debian/os
         :db (db version)
         :client (client nil)
         :model (model/set)
         :generator (gen/phases
                     (setup)
                     (->> (adds)
                          (gen/stagger 1/10)
                          (gen/delay 1)
                          (gen/nemesis
                           (gen/seq (cycle
                                     [(gen/sleep 40)
                                      {:type :info :f :start}
                                      (gen/sleep 20)
                                      {:type :info :f :stop}])))
                          (gen/time-limit 600))
                     (recover)
                     (read-once))
         :nemesis nemesis/noop
         :checker checker/set))

;; Randomly pauses (STOP) and resumes (CONT) nodes on the network
(defn hammer-test
  [version]
  (assoc (simple-test version)
         :nemesis (nemesis/hammer-time :stellar-core)))

;; Randomly partitions network into [2 nodes] <-> bridge-node <-> [2 nodes]
(defn bridge-test
  [version]
  (assoc (simple-test version)
         :nemesis (nemesis/partitioner (comp nemesis/bridge shuffle))))

;; Cuts links such that each node sees a different majority
(defn majorities-ring-test
  [version]
  (assoc (simple-test version)
         :nemesis (nemesis/partition-majorities-ring)))

;; Randomly partitions network into halves
(defn random-split-test
  [version]
  (assoc (simple-test version)
         :nemesis (nemesis/partition-random-halves)))

;; Randomly split a single node off the group
(defn random-isolate-test
  [version]
  (assoc (simple-test version)
         :nemesis (nemesis/partition-random-node)))
