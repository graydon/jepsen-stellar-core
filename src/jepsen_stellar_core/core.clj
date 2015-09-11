(ns jepsen-stellar-core.core
  (:use [clojure.core.strint])
  (:require [clj-http.client :as http]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure.tools.logging :refer [warn info debug]]
            [jepsen.os.debian :as debian]
            [jepsen.control.net :as net]
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

(defn stellar-core-deb []
  (<< "stellar-core-~{*stellar-core-version*}_amd64.deb"))

(defn stellar-core-deb-url []
  (<< "~{stellar-core-deb-url-path}/~(stellar-core-deb)"))



;; Basic mechanisms for talking to a stellar-core node's test port

(def ten-million 10000000)
(def ^:dynamic *node-host* "localhost")
(def ^:dynamic *node-port* 11626)

(defn get-json [path & [qp]]
  (let [params {:as :json}]
    (http/get (<< "http://~{*node-host*}:~{*node-port*}/~{path}")
              (if qp
                (assoc params :query-params qp)
                params))))


(defn node-info []
  (-> (get-json "info") :body :info))

(defn ledger-num []
  (-> (node-info) :ledger :num))

(defn node-status []
  (:state (node-info)))

(defn node-metrics []
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

(defn retry-until
  "Keep trying the provided `:f` function until the `:until` function
   passes, or `:ledgers` expire, or `:retries` retries expire. Logs
   some waiting-for-close noise after the first 20 retries, and slows
   retrying down a bit. Returns `:ok` if it passed, `:fail` if anything
   timed out."
  [& {:keys [ledgers retries f until]
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
            (info (<< "awaiting close: node ~{*node-host*}, ledger ~{curr}, status ~(node-status)"))
            (recur (inc retried)))
          true
          (do
            (Thread/sleep 300)
            (recur (inc retried))))))))

(defn install!
  "Install stellar-core and its dependencies on a node."
  [node]
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
             (= (debian/installed-version :stellar-core)
                *stellar-core-version*))
    (c/exec :wget :--no-clobber (stellar-core-deb-url))
    (meh (debian/uninstall! :stellar-core))
    (c/exec :dpkg :-i (stellar-core-deb))))

(defn configure!
  "Install keys, init control and config files on a node.
   Pubkeys are needed to scp history between nodes' history stores."
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
  "Wipe a node's state, keys and config files."
  []
  (meh (c/exec :service :stellar-core :stop))
  (c/exec :rm :-f
          "/etc/init.d/stellar-core"
          "/root/.ssh/known_hosts"
          "/root/.ssh/id_rsa"
          "/root/.ssh/id_rsa.pub")
  (c/exec :rm :-rf :history :buckets :stellar.db :stellar-core.cfg :stellar-core.log))

(defn initialize!
  "Initialize history and database on a node."
  [node]
  (c/exec :stellar-core :--conf :stellar-core.cfg :--newhist node)
  (c/exec :stellar-core :--conf :stellar-core.cfg :--newdb)
  (c/exec :stellar-core :--conf :stellar-core.cfg :--forcescp))


(defn db
  "Standard db/DB reification, for a single node."
  []
  (reify db/DB
    (setup! [db test node]
      (wipe!)
      (install! node)
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


(defn client
  "Standard client for `:add` and `:read` operations, modeled by `model/set`.
  
  An `{:type :invoke :f :add :value n}` operation causes this client
  to add an account named `account~{n}` from a funding-account called
  `(nodenum n)`. That is, `account0`, `account5`, `account10`, etc. are all
  funded from an intermediate account called `n1`.
  
  Splitting the adds up with intermediate accounts allows them to proceed without
  interfering with one another's sequence numbers; if we didn't do this, most adds
  would fail (harmlessly, but it's a waste of requests; we want to test adds, not
  rejections).
  "
  [node]
  (reify client/Client
    (setup!    [this test node] (client node))
    (teardown! [this test])
    (invoke!   [this test op]
      (case (:f op)

        :setup
        (binding [*node-host* (name node)]
          (let [n (nth nodenames (:value op))]
            (assoc op :type (retry-until
                             :f #(do-create-account n)
                             :until #(has-account n)))))

        :read
        (binding [*node-host* (name node)]
          (assoc op
                 :type :ok,
                 :value (apply sorted-set
                               (filter #(has-account (account-id %1))
                                       (range (inc @max-account))))))

        :add
        (binding [*node-host* (name node)]
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

(defn random-subset
  "Return a random subset of a collection"
  [coll]
  (take (rand-int (inc (count coll))) (shuffle coll)))

(defn damaged-net-nemesis
  "Induces network damage on random subset of nodes"
  [damage & [nodes]]
  (reify client/Client
    (setup! [this test _]
      (let [nodes (random-subset (:nodes test))]
        (c/on-many nodes (meh (net/fast)))
        (damaged-net-nemesis damage nodes)))

    (invoke! [this test op]
      (case (:f op)

        :start
        (do
          (c/on-many nodes (damage))
          (assoc op :value (str "network damaged on " (pr-str nodes))))

        :stop
        (do
          (c/on-many nodes (meh (net/fast)))
          (assoc op :value (str "healed network on " (pr-str nodes))))
        ))

    (teardown! [this test]
      (c/on-many nodes (meh (net/fast)))
      this)))

(defn flaky-net-nemesis [] (damaged-net-nemesis net/flaky))
(defn slow-net-nemesis [] (damaged-net-nemesis net/slow))

(defn simple-test
  "All our tests follow a simple phase structure: they `:setup` up a new network,
  run for 10 minutes `:add`ing sequential accounts and running nemesis disruptions
  for 20 seconds every minute (with 40s to recover from each). The network
  is then allowed to fully heal, and a final `:read` is performed."
  []
  (assoc tests/noop-test
         :name "stellar-core"
         :os debian/os
         :db (db)
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

(defn hammer-test
  "Randomly pauses (STOP) and resumes (CONT) nodes on the network"
  []
  (assoc (simple-test)
         :name "hammer-test"
         :nemesis (nemesis/hammer-time :stellar-core)))

(defn bridge-test
  "Randomly partitions network into [2 nodes] <-> bridge-node <-> [2 nodes]"
  []
  (assoc (simple-test)
         :name "bridge-test"
         :nemesis (nemesis/partitioner (comp nemesis/bridge shuffle))))

(defn majorities-ring-test
  "Cuts links such that each node sees a different majority"
  []
  (assoc (simple-test)
         :name "majorities-ring-test"
         :nemesis (nemesis/partition-majorities-ring)))

(defn random-split-test
  "Randomly partitions network into halves"
  []
  (assoc (simple-test)
         :name "random-split-test"
         :nemesis (nemesis/partition-random-halves)))

(defn random-isolate-test
  "Randomly split a single node off the group"
  []
  (assoc (simple-test)
         :name "random-isolate-test"
         :nemesis (nemesis/partition-random-node)))

(defn flaky-net-test
  "Make network connections randomly flaky"
  []
  (assoc (simple-test)
         :name "flaky-net-test"
         :nemesis (flaky-net-nemesis)))

(defn slow-net-test
  "Make network connections randomly slow"
  []
  (assoc (simple-test)
         :name "slow-net-test"
         :nemesis (slow-net-nemesis)))
