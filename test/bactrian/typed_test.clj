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

(defadt [[x :variance :covariant]] Maybe3
  nothing3 []
  just3 [val :- x])

(defadt [[x :variance :covariant]] Maybe4
  nothing4 []
  just4 [x])

(t/ann j AnyMaybe)
(def j (just :val 8))
(t/ann n AnyMaybe)
(def n (nothing))

(t/ann j2 AnyMaybe2)
(def j2 (just2 8))
(t/ann n2 AnyMaybe2)
(def n2 (nothing2))

(t/ann j3 (AnyMaybe3 t/AnyInteger))
(def j3 (just3 :val 8))
(t/ann n3 (AnyMaybe3 t/AnyInteger))
(def n3 (nothing3))

(t/ann j4 (AnyMaybe4 t/AnyInteger))
(def j4 (just4 8))
(t/ann n4 (AnyMaybe4 t/AnyInteger))
(def n4 (nothing4))

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

(t/ann foo3 [(AnyMaybe3 t/AnyInteger) -> t/AnyInteger])
(defn foo3 [m]
  (match m
    {:just3 {:val x}} x
    {:nothing3 {}} 0))

(t/ann foo4 [(AnyMaybe4 t/AnyInteger) -> t/AnyInteger])
(defn foo4 [m]
  (match m
    {:just4 [x]} x
    {:nothing4 []} 0))

(t/tc-ignore
 (deftest check-ns-test
   (testing "typed"
     (is (t/check-ns 'bactrian.typed)))
   (testing "typed-test"
     (is (t/check-ns 'bactrian.typed-test))))

 (deftest maybe-test
   (testing "just"
     (is (= 8 (foo j))))
   (testing "nothing"
     (is (= 0 (foo n)))))

 (deftest maybe2-test
   (testing "just2"
     (is (= 8 (foo2 j2))))
   (testing "nothing2"
     (is (= 0 (foo2 n2)))))

 (deftest maybe3-test
   (testing "just3"
     (is (= 8 (foo3 j3))))
   (testing "nothing3"
     (is (= 0 (foo3 n3)))))

 (deftest maybe4-test
   (testing "just4"
     (is (= 8 (foo4 j4))))
   (testing "nothing4"
     (is (= 0 (foo4 n4))))))
