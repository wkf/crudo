(ns crudo.core-old
  (:refer-clojure :exclude [find type])
  (:require [schema.core :as s]
            [clojure.string :as string]
            [clojure.core.match :refer [match]]
            [datomic.api :as d :refer [db q]]))

;;
;; #TODO:
;;   rewrite rule builder to generate finer-grained rules
;;   refactor creator method for map-entries
;;   implement finder (query builder)
;;   implement updater
;;   implement deleter
;;   implement a generator for test data (maybe?)
;;

(defprotocol Entity
  (extractor [this] "return function taking db and id and returning domain model")
  (creator [this] "return function taking domain model and returning transaction")
  (validator [this] "return function taking domain model and returning true if valid")

  (installer [this] "return function for building transaction installing model schema")
  (finder [this] "return function for building query reading instance")
  (updater [this] "return function for building transaction updating instance")
  (deleter [this] "return function for building transaction deleting instance "))

(defprotocol Attribute
  (type [this] "datomic attribute data type")
  (value [this] "datomic attribute value")
  (cardinality [this] "datomic schema cardinality"))

(defrecord ExtraMapEntry [kspec val-schema])
(defrecord ExplicitMapEntry [kspec val-schema])

(defn extra-map-entry [kspec val-schema]
  (ExtraMapEntry. kspec val-schema))

(defn explicit-map-entry [kspec val-schema]
  (ExplicitMapEntry. kspec val-schema))

(defn- ref? [attr]
  (= (:type attr) :db.type/ref))

(defn- one? [attr]
  (= (:cardinality attr) :db.cardinality/one))

(defn- many? [attr]
  (= (:cardinality attr) :db.cardinality/many))

(defn var-name [ns & components]
  (string/join "-" (cons (string/replace ns #"\." "-") components)))

(defn var-name-from-attr [ns attr]
  (apply var-name ns (filter identity [(:name attr) (when (ref? attr) "id")])))

(defn constraint-var [ns & components]
  (symbol (str "?" (apply var-name ns components))))

(defn constraint-var-from-attr [ns attr]
  (symbol (str "?" (var-name-from-attr ns attr))))

(defn parent-ns [ns]
  (string/replace ns #"\..*$" ""))

(defn map-walker [m]
  (let [extra-schema (first
                       (remove s/specific-key? (keys m)))
        extra-walker (when extra-schema
                       (s/subschema-walker
                         (apply extra-map-entry (clojure.core/find m extra-schema))))
        explicit-schema (dissoc m extra-schema)
        explicit-walkers (into {} (for [[k v] explicit-schema]
                                    [(s/explicit-schema-key k)
                                     (s/subschema-walker (explicit-map-entry k v))]))]
    (fn
      ([ctx]
       (mapcat
         #(% ctx) (filter identity
                          (conj
                            (vals explicit-walkers) extra-walker))))
      ([ctx data]
       (mapcat
         (fn [[k v]]
           (if-let [walker (explicit-walkers k)]
             (walker ctx [k v])
             (extra-walker ctx [k v])))
         data)))))

(defn map-entry-walker [{:keys [kspec val-schema]} f]
  (let [walker (s/subschema-walker val-schema)
        attr-name (and
                    (s/specific-key? kspec)
                    (name
                      (s/explicit-schema-key kspec)))
        attr-type (type val-schema)
        attr-cardinality (cardinality val-schema)]
    (fn [ctx & data]
      (apply f (assoc ctx
                      :walker walker
                      :attr {:name attr-name
                             :type attr-type
                             :cardinality attr-cardinality}) data))))

(defn seq-walker [this]
  (let [walkers (doall (map s/subschema-walker this))]
    (fn
      ([ctx]
       (mapcat #(% ctx) walkers))
      ([ctx data]
       (mapcat #((first walkers) ctx %) data)))))

;; TODO:
;;  - instead of collapsing rules ad-hoc by passing parent cardinality, make it the parent's responsibility to collapse child rules
;;  - make ns an array

(defn ->qualified-symbol
  ([ns* s]
   (->qualified-symbol ns* s ""))
  ([ns* s prefix]
   (if (symbol? s)
     s
     (symbol
       (str prefix (string/join "-" (filter #(and (identity %) (not= "" %)) (conj ns* s))))))))

(defn ->qualified-keyword [ns* k]
  (let [components (filter identity (conj ns* k))]
    (if (keyword? k)
      k
      (keyword
        (if (> (count components) 1)
          (str
            (string/join "." (butlast components)) "/" (last components))
          (first components))))))

(defn qualify-rule [ns rule]
  (let [ns* (string/split ns #"\.")
        head (first rule)
        constraints (rest rule)
        head* (list*
                (->qualified-symbol ns* (first head))
                (map
                  #(->qualified-symbol ns* % "?") (rest head)))
        constraints* (map
                       (fn [[e a v]]
                         [(->qualified-symbol ns* e "?")
                          (->qualified-keyword ns* a)
                          (->qualified-symbol ns* v "?")])
                       constraints)]
    (apply vector head* constraints*)))

#_(->qualified-symbol ["hello" "world"] "id" "?")
#_(->qualified-keyword ["hello" "world"] "id")

(defn var-name* [attr]
  (if (ref? attr)
    (str (:name attr) "-id")
    (:name attr)))

(extend-type java.lang.Class
  Entity
  (installer [_] (constantly []))
  (finder [_] (constantly []))
  (creator [_] (constantly []))

  Attribute
  (type [this]
    (condp = this
      java.lang.String :db.type/string
      java.lang.Number :db.type/number))
  (cardinality [this] :db.cardinality/one))

(extend-type schema.core.EnumSchema
  Entity
  (installer [{:keys [vs]}]
    (fn [{:keys [ns ids]}]
      (map #(hash-map
              :db/id (d/tempid :db.part/user) :db/ident (keyword ns (name %))) vs)))
  (finder [_]
    (fn [{:keys [ns]}]
      [(qualify-rule ns [["" "id" ""] ["id" :db/ident ""]])]))
  (creator [_]
    (fn [{:keys [ns]} _]
      []))

  Attribute
  (type [this] :db.type/ref)
  (cardinality [this] :db.cardinality/one))

(defn simplify-child-rules
  "if the first id of the child rule is a ref to an attribute with a cardinality of one, inject the corresponding parent rule"
  [{:keys [attr ns]} child-rules parent-rules]
  (if (one? attr)
    (let [ns* (string/split ns #"\.")
          rule-name (var-name* attr)
          [[[child-rule-name id-symbol & vars] & constraints] & rules] child-rules]
      (if (#{(->qualified-symbol ns* rule-name "?")} id-symbol)
        (apply vector
               (apply vector
                      (list* child-rule-name (->qualified-symbol ns* "id" "?") vars)
                      (get-in parent-rules [0 0])
                      constraints)
               rules)
        child-rules))
    child-rules))

(defn map-entry-creator [{:keys [id ns attr walker] :as ctx} [k v]]
  (let [mapper (if (one? attr)
                 (fn [f v] (f v))
                 (fn [f v] (mapcat #(f [%]) v)))
        v* (cond
             ;; FIXME: should I qualify enum VALUES like this?
             (and (ref? attr)
                  (keyword? v)) (keyword (str ns "." (:name attr)) (name v))
             (ref? attr) (d/tempid :db.part/user)
             :else v)
        ctx* (assoc ctx
                    :id (if (ref? attr) v* id)
                    :ns (str ns "." (:name attr)))]
    (mapper #(concat
               (walker ctx* %)
               (if (:name attr)
                 [[:db/add id (keyword ns (:name attr)) v*]]
                 (let [pair-id (d/tempid :db.part/user)]
                   [[:db/add id (keyword ns "-pair") pair-id]
                    [:db/add pair-id (keyword ns "-key") k]
                    [:db/add pair-id (keyword ns "-value") v*]]))) v)))

(extend-type ExplicitMapEntry
  Entity
  (installer [this]
    (map-entry-walker
      this
      (fn [{:keys [ns attr walker] :as ctx}]
        (concat
          (walker
            (assoc ctx :ns (str ns "." (:name attr))))
          [{:db/id (d/tempid :db.part/db)
            :db/ident (keyword ns (:name attr))
            :db/isComponent (= (:type attr) :db.type/ref)
            :db/valueType (:type attr)
            :db/cardinality (:cardinality attr)
            :db.install/_attribute :db.part/db}]))))
  (finder [this]
    (map-entry-walker
      this
      (fn [{:keys [ns attr walker] :as ctx}]
        (let [child-rules (walker
                            (assoc ctx :ns (str ns "." (:name attr))))
              rule-name (var-name* attr)
              parent-rules [(qualify-rule
                              ns
                              [[rule-name "id" rule-name]
                               ["id" (:name attr) rule-name]])]
              child-rules (simplify-child-rules ctx child-rules parent-rules)]
          (concat child-rules parent-rules)))))
  (creator [this]
    (map-entry-walker this map-entry-creator)))

(extend-type ExtraMapEntry
  Entity
  (installer [this]
    (map-entry-walker
      this
      (fn [{:keys [ns attr walker] :as ctx}]
        (concat
          (walker
            (assoc ctx :ns (str ns "." (:name attr))))
            [{:db/id (d/tempid :db.part/db)
              :db/ident (keyword ns "-pair")
              :db/valueType :db.type/ref
              :db/cardinality :db.cardinality/many
              :db.install/_attribute :db.part/db}
             {:db/id (d/tempid :db.part/db)
              :db/ident (keyword ns "-key")
              :db/valueType :db.type/string
              :db/cardinality :db.cardinality/one
              :db.install/_attribute :db.part/db}
             {:db/id (d/tempid :db.part/db)
              :db/ident (keyword ns "-value")
              :db/isComponent (= (:type attr) :db.type/ref)
              :db/valueType (:type attr)
              :db/cardinality (:cardinality attr)
              :db.install/_attribute :db.part/db}]))))
  (finder [this]
    (map-entry-walker
      this
      (fn [{:keys [ns attr walker] :as ctx}]
        (concat
          (walker
            (assoc ctx :ns (str ns "." (:name attr))))
          [(qualify-rule
             ns
             [["" "id" "-key" "-value"]
              ["id" "-pair" "-pair-id"]
              ["-pair-id" "-key" "-key"]
              ["-pair-id" "-value" "-value"]])]))))
  (creator [this]
    (map-entry-walker this map-entry-creator)))

(extend-type clojure.lang.PersistentVector
  Entity
  (installer [this] (seq-walker this))
  (finder [this] (seq-walker this))
  (creator [this] (seq-walker this))

  Attribute
  (type [this]
    (when-first [that this]
      (let [t (type that)
            c (cardinality that)]
      (cond
        (= t :db.type/ref) :db.type/ref
        (= c :db.cardinality/many) :db.type/ref
        :else t))))
  (cardinality [_] :db.cardinality/many))

(extend-type clojure.lang.PersistentHashMap
  Entity
  (installer [this] (map-walker this))
  (finder [this] (map-walker this))
  (creator [this] (map-walker this))

  Attribute
  (type [_] :db.type/ref)
  (cardinality [_] :db.cardinality/one))

(extend-type clojure.lang.PersistentArrayMap
  Entity
  (installer [this] (map-walker this))
  (finder [this] (map-walker this))
  (creator [this] (map-walker this))

  Attribute
  (type [_] :db.type/ref)
  (cardinality [_] :db.cardinality/one))

(defn model [ns schema]
  {:ns ns
   :schema schema})

(defmacro defmodel [name & {:as schema}]
  `(def ~name
     (model ~(keyword name) ~schema)))

(defn install-tx [model]
  ((s/start-walker installer (:schema model)) {:ns (-> model :ns name)}))

(defn create-tx [model data]
  ((s/start-walker creator (:schema model)) {:ns (-> model :ns name)
                                             :id (d/tempid :db.part/user)} data))

(defn constraint-vars [clauses]
  (distinct
    (reduce
      #(-> %1
           (conj (first %2))
           (conj (last %2)))
      []
      clauses)))

(defn rules [model]
  ((s/start-walker finder (:schema model)) {:ns (-> model :ns name)}))

;; (defn rules [model]
;;   (let [ns (-> model :ns name)
;;         clauses ((s/start-walker finder (:schema model)) {:ns ns})
;;         vars (distinct (constraint-vars clauses))
;;         head (conj (list* vars) (symbol ns))
;;         rule (apply vector head clauses)]
;;     [rule]))

(defn find-q [model]
  (let [ns (-> model :ns name)
        clauses ((s/start-walker finder (:schema model)) {:ns ns})
        vars (distinct (constraint-vars clauses))
        head (conj (list* vars) (symbol ns))
        rule (apply vector head clauses)
        query (concat
                (apply vector :find vars)
                [:in '$ '%]
                [:where head])]
    query))

(comment
  "some example queries"

  (d/create-database "datomic:mem://crudo-dev")
  (d/delete-database "datomic:mem://crudo-dev")

  [db task]

  (do
    (doseq [k #{:task :type :state :repo}]
      (when-not (k task)
        (throw (IllegalArgumentException. (str "Missing task " k)))))
    (let [e (tempid)]
      (conj (when (:params task)
              (map (fn [[k v]]
                     {:db/id (tempid)
                      :task/_params e
                      :task.params/-key k
                      :task.params/-value v})
                   (:params task)))
            {:db/id e
             :task/type (:type task)
             :task/state (:state task)
             :task/repo (:repo task)
             :task/commit (:commit task)})))

  (defmodel task
    :type s/Str
    :state (s/enum :created :running :finished :crashed)
    :repo s/Str
    :commit s/Str
    :params [{s/Keyword s/Str}])

  (defmodel task
    :type s/Str
    :state (s/enum :created :running :finished :crashed)
    :repo s/Str
    :commit s/Str
    :params {s/Keyword s/Str})
)

(comment


  (datomic/ensure-models user task)

  (find task
        {:type "build"
         :params [:hello "world"]}
        [:user])

  (def db-uri "datomic:mem://crudo-dev")

  (defn scratch-conn []
    (d/connect db-uri))

  (defn scratch-db []
    (db (scratch-conn)))

  @(d/transact
     (d/connect "datomic:mem://crudo-dev") (install-tx task))

  @(d/transact
     (d/connect "datomic:mem://crudo-dev")
     (create-tx task {:commit "commit"
                      :repo "repo"
                      :type "build"
                      :state :created
                      :params [{"what" "fun"
                                "hello" "world"}]}))
  (./pprint (rules task))

  (q '[:find ?e ?k ?v
       :in $ %
       :where (task-params ?e ?k ?v)]
     (scratch-db) (rules task))

  (./pprint (installed-attributes (scratch-db)))

  (:vs (s/enum :hello :there))

  (take 1 ids)

  (./pprint task)

  (./pprint (rules task))
  (./pprint (d/entity (scratch-db) 17592186045427))
  (./pprint (install-tx task))
  (./pprint (create-tx task {:commit "commit"
                             :repo "repo"
                             :type "build"
                             :state :created
                             :params [{"what" "fun"
                                       "hello" "world"}]}))
  (./pprint (find-q task))

  (./pprint (->rules task*))
  (./pprint (->schema task*))

  (./pprint schema-file)

  (./pprint ((s/start-walker head (:schema task*)) {:ns (-> task* :name name)}))
  (./pprint ((s/start-walker clauses (:schema task*)) {:ns (-> task* :name name)}))
  (./pprint ((s/start-walker schema (:schema task*)) {:ns (-> task* :name name)}))

  ((s/start-walker (fn [s]
                     (let [walk (s/walker s)]
                       (fn [d]
                         (walk d)
                         ))) (:schema task*)) {:commit "commit"
                                               :repo "repo"
                                               :type "build"
                                               :state :created
                                               :params [{:hello "world"}]})
  (./pprint (s/explain {s/Keyword s/Num}))

  (migrate schema)

  (find (scratch-db) :task)
  (find (scratch-db) :task :id 17592186045425)
  (find (scratch-db) :task :type "build" :id 17592186045425)

  )
