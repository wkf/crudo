(ns crudo.core
  (:refer-clojure :exclude [type])
  (:require [clojure.string :as str]
            [schema.core :as s]
            [datomic.api :as d :refer [db q]]))

(declare enum-schema
         type-schema
         static-map-entry
         dynamic-map-entry)

(declare map-type
         map-cardinality
         map-seeds
         map-rules
         map-extractor
         map-creator)

(declare attribute)

;;

(defprotocol Entity
  (seeds [this ns] "returns array of transactions installing necessary entity data")
  (rules [this ns] "returns array of rules to make querying entity data easier")
  (extractor [this] "return function taking db and id and returning domain model")
  (creator [this] "return function taking domain model and returning transaction"))

(defprotocol Attribute
  (attributes [this] "generate transaction for attribute")
  (type [this] "datomic attribute data type")
  (cardinality [this] "datomic schema cardinality"))

(defrecord TypeSchema [t options]
  Entity
  (seeds [_ ns]
    [{:db/valueType t
      :db/cardinality :db.cardinality/one}]))

(defrecord EnumSchema [vs options]
  Entity
  (seeds [_ ns]
    [{:db/valueType :db.type/ref
      :db/cardinality :db.cardinality/one}]))

(defrecord StaticMapEntry [k v]
  Entity
  (seeds [_ ns]
    (let [ns* (str ns "." (str/replace (name k) #"\/" "."))]
      (update-in
        (vec (seeds v ns*)) [0] (partial
                                  merge
                                  {:db/id (d/tempid :db.part/db)
                                   :db/ident k
                                   :db.install/_attribute :db.part/db})))))

(defrecord DynamicMapEntry [k v]
  Entity
  (seeds [_ ns]
    (apply vector
           {:db/id (d/tempid :db.part/db)
            :db/ident (keyword ns "-pair")
            :db/valueType :db.type/ref
            :db/cardinality :db.cardinality/many
            :db.install/_attribute :db.part/db}
           (concat
             (update-in
               (seeds k ns) [0] (partial merge
                                         {:db/id (d/tempid :db.part/db)
                                          :db/ident (keyword ns "-key")
                                          :db/cardinality :db.cardinality/one
                                          :db.install/_attribute :db.part/db}))
             (update-in
               (seeds v ns) [0] #(merge
                                   (attribute (keyword ns "-val") nil :db.cardinality/one)
                                   %))))))

(extend-type clojure.lang.Keyword
  Entity
  (seeds [this ns] [{:db/ident this
                     :db/valueType :db.type/keyword}]))

(extend-type clojure.lang.APersistentVector
  Entity
  (seeds [this ns]
    (let [entry (dynamic-map-entry
                  (reify Entity
                    (seeds [_ ns] [{:db/valueType :db.type/long}]))
                  (first this))]
      (apply vector
             {:db/valueType :db.type/ref
              :db/cardinality :db.cardinality/one}
             (seeds entry ns)))))

(extend-type clojure.lang.PersistentHashMap
  Entity
  (seeds [this ns] (map-seeds this ns)))

(extend-type clojure.lang.PersistentArrayMap
  Entity
  (seeds [this ns] (map-seeds this ns)))

(defn entry-type [[k _]]
  (if (keyword? k) :static :dynamic))

(defn map-seeds [this ns]
  (let [{:keys [static dynamic]} (group-by entry-type this)
        entries (concat
                  (map static-map-entry static)
                  (when-first [d dynamic]
                    [(dynamic-map-entry d)]))]
    (apply vector
           {:db/valueType :db.type/ref
            :db/cardinality :db.cardinality/one}
           (mapcat #(seeds % ns) entries))))

;;

(defn attribute
  ([ident type cardinality]
   (attribute ident type cardinality {}))
  ([ident type cardinality options]
   {:db/id (d/tempid :db.part/db)
    :db/ident ident
    :db/valueType type
    :db/cardinality cardinality
    :db.install/_attribute :db.part/db}))

;; public constructors

(defn enum-schema [vs & {:as options}]
  (EnumSchema. vs (or options {})))

(defn type-schema [t & {:as options}]
  (TypeSchema. t (or options {})))

(defn static-map-entry
  ([[k v]] (static-map-entry k v))
  ([k v] (StaticMapEntry. k v)))

(defn dynamic-map-entry
  ([[k v]] (dynamic-map-entry k v))
  ([k v] (DynamicMapEntry. k v)))

(def schema
  {
   :task/type [(type-schema :db.type/string :indexed? true)]
   :task/state (enum-schema [:task.state/created
                        :task.state/running
                        :task.state/crashed
                        :task.state/finished] :indexed? true :doc "a task's state")
   :task/repo (type-schema :db.type/string :indexed? true :fulltext? true)
   :task/commit (type-schema :db.type/string :indexed? true)
   :task/params {(type-schema :db.type/keyword) (type-schema :db.type/string)}
   (type-schema :db.type/keyword) (type-schema :db.type/string)
   })

#_(seeds crudo.type/schema :task)
#_(type (type-schema :db.type/string :indexed? true))

(comment

  (def db-uri "datomic:mem://crudo-dev")

  (d/create-database "datomic:mem://crudo-dev")
  (d/delete-database "datomic:mem://crudo-dev")

  (defn scratch-conn []
    (d/connect db-uri))

  (defn scratch-db []
    (db (scratch-conn)))

  @(d/transact
     (d/connect "datomic:mem://crudo-dev") (rest (seeds schema "task"))))
