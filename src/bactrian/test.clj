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

(t/check-ns)
