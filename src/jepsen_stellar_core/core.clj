(ns jepsen-stellar-core.core
  (:use [clojure.core.strint])
  (:require [clj-http.client :as http]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.tools.logging :refer [warn info debug]]
            [jepsen.os.debian :as debian]
            [jepsen [client :as client]
             [core :as jepsen]
             [db :as db]
             [tests :as tests]
             [control :as c :refer [|]]
             [checker :as checker]
             [nemesis :as nemesis]
             [generator :as gen]
             [util :refer [timeout meh]]]))

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

(defn server-metrics []
  (-> (get-json "metrics") :body :metrics))

(defn test-tx [qp]
  (-> (get-json "testtx" qp) :body))

(defn get-account [who]
  (-> (get-json "testacc" {:name who}) :body))


(defn payment-qp [from to amount]
  {:from from
   :to to
   :amount amount})

(defn create-account-qp [to]
  (assoc (payment-qp "root" to (* 1000 ten-million))
         :create 'true))

(defn do-payment [from to amount]
  (test-tx (payment-qp from to amount)))

(defn do-create-account [to]
  (test-tx (create-account-qp to)))



(defn install!
  [node version]
  (when-not (debian/installed? :libpq5:amd64)
    (debian/update!)
    (debian/install '(:libpq5:amd64)))
  (when-not (debian/installed? :libsqlite3-0:amd64)
    (debian/update!)
    (debian/install '(:libsqlite3-0:amd64)))
  (when-not (debian/installed? :stellar-core:amd64)
    (c/exec :wget :--no-clobber (stellar-core-deb-url version))
    (c/exec :dpkg :-i (stellar-core-deb version))))

(defn configure!
  [node]
  (let [self (nodes node)
        others (vec (sort (filter #(not= %1 node) (keys nodes))))]

    (c/upload "/root/.ssh/known_hosts"
              "/root/.ssh/id_rsa"
              "/root/.ssh/id_rsa.pub"
              "/root/.ssh")
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
  (try (c/exec :service :stellar-core :stop))
  (if (debian/installed? :stellar-core)
    (debian/uninstall! :stellar-core))
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
      (jepsen/synchronize test)
      (c/exec :service :stellar-core :start))

    (teardown! [db test node]
      (try (c/exec :service :stellar-core :stop)))
    ))


;; For jepsen to apply operations, we must reify client/Client
(defn client
  [node]
  (reify client/Client
    (setup!    [this test node] (client node))
    (teardown! [this test])
    (invoke!   [this test op]
      (case (:f op)

        :read
        (binding [*server-host* node]
          (assoc op
                 :type :ok,
                 :value (get-account (:id op))))

        :add
        (binding [*server-host* node]
          (do-create-account (:id op))
          (assoc op :type :ok))

        :transfer
        (binding [*server-host* node]
          (let [{:keys [from to amount]} op]
            (do-payment from to amount)
            (assoc op :type :ok)))

        ))))


(defn simple-test
  [version]
  (assoc tests/noop-test
         :name "stellar-core"
         :os debian/os
         :db (db version)
         :client (client nil)
         :generator (gen/concat
                     (gen/once {:type :info, :f :start})
                     (gen/once {:type :invoke,
                                :f :add,
                                :id :account1})
                     (gen/once {:type :info, :f :stop})
                     )
         :nemesis nemesis/noop
         :checker checker/unbridled-optimism))
