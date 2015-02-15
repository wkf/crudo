(ns crudo.type
  (:refer-clojure :exclude [keyword boolean long bigint float double bigdec ref bytes])
  (:require [crudo.core :as c]))

(def string (partial c/type-schema :db.type/string))
(def keyword (partial c/type-schema :db.type/keyword))
(def boolean (partial c/type-schema :db.type/boolean))
(def long (partial c/type-schema :db.type/long))
(def bigint (partial c/type-schema :db.type/bigint))
(def float (partial c/type-schema :db.type/float))
(def double (partial c/type-schema :db.type/double))
(def bigdec (partial c/type-schema :db.type/bigdec))
(def ref (partial c/type-schema :db.type/ref))
(def instant (partial c/type-schema :db.type/instant))
(def uuid (partial c/type-schema :db.type/uuid))
(def uri (partial c/type-schema :db.type/uri))
(def bytes (partial c/type-schema :db.type/bytes))

(def enum #'c/enum-schema)

(defn seeds [n schema] (c/seeds schema (name n)))
