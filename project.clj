(defproject crudo "0.1.0-SNAPSHOT"
  :description "Schema driven datomic transaction/query builder."
  :url "https://github.com/wkf/crudo"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.clojure/core.match "0.2.1"]
                 [prismatic/schema "0.2.6"]
                 [com.datomic/datomic-free "0.9.4899"]])
