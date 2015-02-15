(ns crudo.datomic-helper
  (:require [datomic.api :as d :refer [db q]]))

(def ^:dynamic *uri* "datomic:mem://crudo-test")

(defn scratch-conn []
  (d/connect *uri*))

(defn scratch-db []
  (db (scratch-conn)))

(defn create-database []
  (d/create-database *uri*))

(defn delete-database []
  (d/delete-database *uri*))
