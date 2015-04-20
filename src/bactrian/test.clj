(ns bactrian.test
  (:require [bactrian.typed :refer :all]
            [clojure.core.typed :as t]
            [clojure.core.match :refer [match]]))

(defadt Maybe
  nothing []
  just [val :- t/AnyInteger])

(defadt Maybe2
  nothing2 []
  just2 [t/AnyInteger])

(defadt [[x :variance :covariant]] Maybe3
  nothing3 []
  just3 [val :- x])

(defadt [[x :variance :covariant]] Maybe4
  nothing4 []
  just4 [x])

(t/ann t1 AnyMaybe)
(def t1 (just :val 8))

(t/ann tst [AnyMaybe -> t/AnyInteger])
(defn tst [j]
  (match j
    {:just {:val a}} a
    {:nothing {}} 0))


(t/ann t2 AnyMaybe2)
(def t2 (just2 8))

(t/ann tst2 [AnyMaybe2 -> t/AnyInteger])
(defn tst2 [j]
  (match j
    {:just2 [a]} a
    {:nothing2 []} 0))

(t/ann n3 (AnyMaybe3 t/AnyInteger))
(def n3 (nothing3 ))

(t/ann j3 (AnyMaybe3 t/AnyInteger))
(def j3 (just3 :val 8))

(t/ann tst3 [(AnyMaybe3 t/AnyInteger) -> t/AnyInteger])
(defn tst3 [j]
  (match j
    {:just3 {:val a}} a
    {:nothing3 {}} 0))

(t/ann n4 (AnyMaybe4 t/AnyInteger))
(def n4 (nothing4))

(t/ann j4 (AnyMaybe4 t/AnyInteger))
(def j4 (just4 8))

(t/ann tst4 [(AnyMaybe4 t/AnyInteger) -> t/AnyInteger])
(defn tst4 [j]
  (match j
    {:just4 [a]} a
    {:nothing4 []} 0))

(t/check-ns)
