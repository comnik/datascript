(ns datascript.js
  (:require-macros
   [datascript.mori.macros :refer [mori-export]])
  (:refer-clojure 
    :exclude
      [filter count empty conj find nth assoc dissoc disj pop peek get
       empty? reverse into merge subvec keys vals
       equiv sort sort-by
       vector vec array-map hash-map set compare
       shuffle])
  (:require
    [datascript.core :as d]
    [datascript.transit :as dt]
    [clojure.walk :as walk]
    [cljs.reader]))

;; Conversions

(defn- keywordize [s]
  (if (and (string? s) (= (subs s 0 1) ":"))
    (keyword (subs s 1))
    s))

(defn- schema->clj [schema]
  (->> (js->clj schema)
       (reduce-kv
         (fn [m k v] (cljs.core/assoc m k (walk/postwalk keywordize v))) {})))

(declare entities->clj)

(defn- entity->clj [e]
  (cond (map? e)
    (-> e
      (cljs.core/dissoc ":db/id")
      (cljs.core/assoc  :db/id (e ":db/id")))
    (= (first e) ":db.fn/call")
      (let [[_ f & args] e]
        (concat [:db.fn/call (fn [& args] (entities->clj (apply f args)))] args))
    (sequential? e)
      (let [[op & entity] e]
        (concat [(keywordize op)] entity))))

(defn- entities->clj [entities]
  (->> (js->clj entities)
       (map entity->clj)))

(defn- tempids->js [tempids]
  (let [obj (js-obj)]
    (doseq [[k v] tempids]
      (aset obj (str k) v))
    obj))

(defn- tx-report->js [report]
  #js { :db_before (:db-before report)
        :db_after  (:db-after report)
        :tx_data   (->> (:tx-data report) into-array)
        :tempids   (tempids->js (:tempids report))
        :tx_meta   (:tx-meta report) })

(defn js->Datom [d]
  (if (array? d)
    (d/datom (aget d 0) (aget d 1) (aget d 2) (or (aget d 3) d/tx0) (or (aget d 4) true))
    (d/datom (.-e d) (.-a d) (.-v d) (or (.-tx d) d/tx0) (or (.-added d) true))))

(defn- pull-result->js
  [result]
  (->> result
       (walk/postwalk #(if (keyword? %) (str %) %))))
       ; clj->js))

;; Public API

(defn ^:export empty_db [& [schema]]
  (d/empty-db (schema->clj schema)))

(defn ^:export init_db [datoms & [schema]]
  (d/init-db (map js->Datom datoms) (schema->clj schema)))

(defn ^:export q [query & sources]
  (let [query   (cljs.reader/read-string query)
        results (apply d/q query sources)]
    (clj->js results)))

(defn ^:export pull [db pattern eid]
  (let [pattern (cljs.reader/read-string pattern)
        eid (js->clj eid)
        results (d/pull db pattern eid)]
    (pull-result->js results)))

(defn ^:export pull_many [db pattern eids]
  (let [pattern (cljs.reader/read-string pattern)
        eids (js->clj eids)
        results (d/pull-many db pattern eids)]
    (pull-result->js results)))

(defn ^:export db_with [db entities]
  (d/db-with db (entities->clj entities)))

(defn ^:export entity [db eid]
  (d/entity db (js->clj eid)))

(def ^:export touch       d/touch)
(def ^:export entity_db   d/entity-db)
(def ^:export filter      d/filter)
(def ^:export is_filtered d/is-filtered)

(defn ^:export create_conn [& [schema]]
  (d/create-conn (schema->clj schema)))

(def ^:export conn_from_db d/conn-from-db)

(defn ^:export conn_from_datoms
  ([datoms]        (conn_from_db (init_db datoms)))
  ([datoms schema] (conn_from_db (init_db datoms schema))))

(defn ^:export db [conn]
  @conn)

(defn ^:export transact [conn entities & [tx-meta]]
  (let [entities (entities->clj entities)
        report   (-> (d/-transact! conn entities tx-meta)
                     tx-report->js)]
    (doseq [[_ callback] @(:listeners (meta conn))]
      (callback report))
    report))

(defn ^:export reset_conn [conn db & [tx-meta]]
  (let [report #js { :db_before @conn
                     :db_after  db
                     :tx_data   (into-array
                                  (concat
                                    (map #(cljs.core/assoc % :added false) (d/datoms @conn :eavt))
                                    (d/datoms db :eavt)))
                     :tx_meta   tx-meta }]
    (reset! conn db)
    (doseq [[_ callback] @(:listeners (meta conn))]
      (callback report))
    db))

(def ^:export listen d/listen!)

(def ^:export unlisten d/unlisten!)

(defn ^:export resolve_tempid [tempids tempid]
  (aget tempids (str tempid)))

(defn ^:export datoms [db index & components]
  (->> (apply d/datoms db (keywordize index) components)
       into-array))

(defn ^:export seek_datoms [db index & components]
  (->> (apply d/seek-datoms db (keywordize index) components)
       into-array))

(defn ^:export index_range [db attr start end]
  (into-array (d/index-range db attr start end)))

(defn ^:export squuid []
  (str (d/squuid)))

(defn ^:export squuid_time_millis [uuid]
  (d/squuid-time-millis (cljs.core/uuid uuid)))

(defn ^:export read_transit_str [s]
  (dt/read-transit-str s))

(defn ^:export write_transit_str [db]
  (dt/write-transit-str db))

;; Keywords

(def ^:export DB_ID :db/id)
(def ^:export DB_FN_CALL :db.fn/call)
(def ^:export DB_BEFORE :db-before)
(def ^:export DB_AFTER :db-after)
(def ^:export TX_DATA :tx-data)
(def ^:export TEMPIDS :tempids)
(def ^:export TX_META :tx-meta)
(def ^:export EAVT :eavt)
(def ^:export AEVT :aevt)
(def ^:export AVET :avet)
(def ^:export DB_PART_TX :db.part/tx)
(def ^:export DB_CURRENT_TX :db/current-tx)
(def ^:export DB_CARDINALITY :db/cardinality)
(def ^:export DB_CARDINALITY_MANY :db.cardinality/many)
(def ^:export DB_UNIQUE :db/unique)
(def ^:export DB_IDENT :db/ident)
(def ^:export DB_UNIQUE_IDENTITY :db.unique/identity)
(def ^:export DB_ADD :db/add)
(def ^:export DB_RETRACT :db/retract)
(def ^:export DB_RETRACT_ENTITY :db.fn/retractEntity)
(def ^:export DB_RETRACT_ATTRIBUTE :db.fn/retractAttribute)
(def ^:export DB_VALUE_TYPE :db/valueType)
(def ^:export DB_TYPE_REF :db.type/ref)
(def ^:export DB_IS_COMPONENT :db/isComponent )
(def ^:export FIND :find)
(def ^:export IN :in)
(def ^:export WHERE :where)
(def ^:export ADDED :added)
(def ^:export E :e)
(def ^:export A :a)
(def ^:export V :v)

;; Mori

(mori-export count cljs.core/count)
(mori-export empty cljs.core/empty)
(mori-export conj cljs.core/conj)
(mori-export find cljs.core/find)
(mori-export nth cljs.core/nth)
(mori-export assoc cljs.core/assoc)
(mori-export dissoc cljs.core/dissoc)
(mori-export disj cljs.core/disj)
(mori-export pop cljs.core/pop)
(mori-export peek cljs.core/peek)
(mori-export get cljs.core/get)
(mori-export isEmpty cljs.core/empty?)
(mori-export reverse cljs.core/reverse)
(mori-export into cljs.core/into)
(mori-export merge cljs.core/merge)
(mori-export subvec cljs.core/subvec)
(mori-export keys cljs.core/keys)
(mori-export vals cljs.core/vals)
(mori-export equiv cljs.core/==)
(mori-export sort cljs.core/sort)
(mori-export sortBy cljs.core/sort-by)
(mori-export vector cljs.core/vector)
(mori-export vec cljs.core/vec)
(mori-export Vector cljs.core/PersistentVector)
(mori-export hashMap cljs.core/array-map)
(mori-export ArrayMap cljs.core/PersistentArrayMap)
(defn ^:export arrayMapFromArray [arr no-clone no-check]
  (cljs.core/PersistentArrayMap.fromArray arr no-clone no-check))
(defn ^:export arrayMapUnwrap [m]
  (if (instance? cljs.core/PersistentArrayMap m)
    (.-arr m)
    (throw (js/Error. "Can only unwrap array maps"))))
(mori-export Map cljs.core/PersistentHashMap)
(mori-export set cljs.core/set)
(mori-export Set cljs.core/PersistentHashSet)
(mori-export compare cljs.core/compare)
(mori-export shuffle cljs.core/shuffle)
