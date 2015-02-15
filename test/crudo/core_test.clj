(ns crudo.core-test
  (:require [clojure.test :refer :all]
            [schema.core :as s]
            [datomic.api :as d :refer [db q]]
            [crudo.core :as c]))

(def db-uri "datomic:mem://crudo-test")

(defn scratch-conn []
  (d/connect db-uri))

(c/defmodel task :type s/Str)

;; (defn type= [t]
;;   (contains {:db/valueType t}))
;;
;; (defn cardinality= [c]
;;   (contains {:db/cardinality c}))
;;
;; (defn ident= [i]
;;   (contains {:db/ident i}))
;;
;; (def one?
;;   (cardinality= :db.cardinality/one))
;;
;; (def many?
;;   (cardinality= :db.cardinality/many))
;;
;; (def component?
;;   (contains {:db/isCompenent true}))
;;
;; (def attribute?
;;   (contains {:db.install/_attribute :db.part/db}))
;;
;; (defn only-valid-keys? [tx]
;;   (every?
;;     #{:db/id
;;       :db/ident
;;       :db/valueType
;;       :db/cardinality
;;       :db.install/_attribute
;;       :db/isComponent} (keys tx)))

(c/defmodel user
  :name s/Str
  :emails [s/Str])

(c/defmodel task
  :type s/Str
  :state (s/enum :created :running :finished :errored)
  :params {s/Str s/Str})

(def users
  [{:name "John Doe"
    :emails ["john@email.gov"
             "banana@gmail.com"]}
   {:name "Jane Dough"
    :emails ["jane@fudge.co"
             "hiccup@celary.co.uk"]}])

(def tasks
  [{:type "build"
    :state :created
    :params {"what" "fun"
             "help" "please"}}
   {:type "deploy"
    :state :finished
    :params {"fruit" "banana"
             "language" "german"}}])

(defn transact [tx]
  (:db-after @(d/transact (scratch-conn) tx)))

(defn install [model]
  (transact (c/install-tx model)))

(defn create [model & data]
  (last
    (for [d data]
      (transact (c/create-tx model d)))))

(defn installed? [db model] true)

(use-fixtures
  :each
  (fn [f]
    (d/create-database db-uri)
    (f)
    (d/delete-database db-uri)))

;; (install task)
;; (apply create task tasks)

;; (./pprint (c/create-tx task {:commit "commit"
;;                              :repo "repo"
;;                              :type "build"
;;                              :state :created
;;                              :params [{"what" "fun"
;;                                        "hello" "world"}]}))


(deftest install-tx
  (testing "a thing"
    (is (when-let [db (install task)] (installed? db task)))))

(run-tests)

;; what should I test?
;; should I install the schema, and then observe the database?
;; is it enough to make assertions about the transactions themselves?
;; I don't care about the transactions, I care about the results when applied to the database

;; (defn install-schema [s]
;;   @(d/transact (scratch-conn) s))
;;
;; (expect nil? 1)

;;
;; (against-background
;;   [(before :contents (d/create-database db-uri))
;;    (after :contents (d/delete-database db-uri))]
;;
;;   (fact "about install tx"
;;         (let [m (model :task
;;                        {:type s/Str})
;;               txs (install-tx m)
;;               [tx] txs]
;;           tx => only-valid-keys?
;;           tx => (ident= :task/type)
;;           tx => (type= :db.type/string)
;;           tx => one?
;;           tx => attribute?
;;           tx =not=> component?
;;
;;           (install-schema txs) => anything
;;
;;
;;           )))
