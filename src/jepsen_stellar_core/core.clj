(ns jepsen-stellar-core.core
  (:use [clojure.core.strint])
  (:require [clj-http.client :as http]
            [clojure.java.io :as io]
            [clojure.string :as str]
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


;; Configuration loading

(defn expand-config [node]
  (let [others (sort (filter (fn [x] (not= x node)) (keys nodes)))
        sec ((nodes node) :sec)]
    (-> (io/resource "stellar-core.cfg")
        slurp
        (str/replace #"%VALIDATION_SEED%" sec)
        (str/replace #"%SELF%" (name node))
        (str/replace #"%OTHER1%" (name (nth others 0)))
        (str/replace #"%OTHER2%" (name (nth others 1)))
        (str/replace #"%OTHER3%" (name (nth others 2)))
        (str/replace #"%OTHER4%" (name (nth others 3)))
        ;; ... more here
        )))


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



;; For jepsen to manage a server, we must reify the db/DB protocol
(defn db
  [version]
  (reify db/DB
    (setup!    [db test node])
    (teardown! [db test node])))


;; For jepsen to apply operations, we must reify client/Client
(def client
  (reify client/Client
    (setup!    [this test node] this)
    (teardown! [this test])
    (invoke!   [this test op] (assoc op :type :ok))))
