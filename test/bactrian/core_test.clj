(ns bactrian.core-test
  (:require [clojure.test :refer :all]
            [bactrian.core :refer :all]
            [clojure.core.match :refer [match]]))

(defadt Maybe
  nothing 0
  just 1)

(defadt Pair
  root []
  pair [car cdr])

(def j (just 8))
(def n (nothing))
(def r (root))
(def p (pair 1 2))

(deftest maybe-test
  (testing "just"
    (is (= 8 (match j [:just x] x [:nothing] 0))))
  (testing "nothing"
    (is (= 0 (match n [:just x] x [:nothing] 0)))))

(deftest pair-test
  (testing "pair"
    (is (= 1 (match p [:pair {:car a, :cdr b}] a [root] 0))))
  (testing "root"
    (is (= 0 (match r [:pair {:car a, :cdr b}] a [root] 0)))))
