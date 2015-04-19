(ns bactrian.typed-test
  (:require [clojure.test :refer :all]
            [bactrian.typed :refer :all]
            [clojure.core.typed :as t]
            [clojure.core.match :refer [match]]))

(defadt Maybe
  nothing []
  just [val :- t/AnyInteger])

(defadt Maybe2
  nothing2 []
  just2 [t/AnyInteger])

(t/ann j AnyMaybe)
(def j (just :val 8))
(t/ann n AnyMaybe)
(def n (nothing))

(t/ann j2 AnyMaybe2)
(def j2 (just2 8))
(t/ann n2 AnyMaybe2)
(def n2 (nothing2))

(t/ann foo [AnyMaybe -> t/AnyInteger])
(defn foo [m]
  (match m
    {:just {:val x}} x
    {:nothing {}} 0))

(t/ann foo2 [AnyMaybe2 -> t/AnyInteger])
(defn foo2 [m]
  (match m
    {:just2 [x]} x
    {:nothing2 []} 0))

(t/tc-ignore
 (deftest check-ns-test
   (testing "typed"
     (is (t/check-ns 'bactrian.typed))))

 (deftest maybe-test
   (testing "just"
     (is (= 8 (foo j))))
   (testing "nothing"
     (is (= 0 (foo n)))))

 (deftest maybe2-test
   (testing "just2"
     (is (= 8 (foo2 j2))))
   (testing "nothing2"
     (is (= 0 (foo2 n2))))))

(t/check-ns)
