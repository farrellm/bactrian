(ns bactrian.test
  (:require [bactrian.types :refer :all]
            [clojure.core.typed :as t]))

(defadt Maybe
  nothing []
  just [value :- t/AnyInteger])

(t/check-ns)
