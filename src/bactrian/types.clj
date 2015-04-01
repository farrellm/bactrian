(ns bactrian.types
  (:refer-clojure :exclude [defprotocol fn])
  (:require [clojure.core.typed :as t]
            [clojure.string :as s]))

#_((t/ann-protocol [[x :variance :covariant]] Maybe)
   (t/defprotocol Maybe)

   (t/ann-record [[x :variance :covariant]] Nothing [+ :- t/Kw])
   (defrecord Nothing [+]
     Maybe)

   (t/ann-record [[x :variance :covariant]] Just [+ :- t/Kw, value :- x])
   (defrecord Just [+ value]
     Maybe))

;; (t/ann kw->sym [t/Kw -> t/Sym])
;; (defn- kw->sym [kw]
;;   (-> kw name symbol))

(t/tc-ignore
 (defn gen-defs [type-name [name args]]
   (let [rec-name (-> name str s/capitalize symbol)
         arg-types (into [] (concat ['+ :- 't/Kw] args))
         arg-names (map first (partition 3 args))]
     `((t/ann-record ~rec-name ~arg-types)
       (defrecord ~rec-name
           ~(into [] (concat ['+] arg-names))
         ~type-name))))

 (defn defadt* [type-name body]
   (let [defs (mapcat (partial gen-defs type-name)
                      (partition 2 body))]
     `(do (t/ann-protocol ~type-name)
          (t/defprotocol ~type-name)
          ~@defs)))

 (defmacro defadt [name & body]
   (defadt* name body)))


#_(defadt* 'Maybe '(nothing []
                    just [value :- t/Any]))

#_(defadt [[x :variance :covariant]] Maybe
  nothing []
  just [value x])

#_(defadt Maybe
  nothig []n
  just [:value :- t/AnyInteger])

#_(defprotocol Tree)
#_(defrecord Leaf []
#_  Tree)
#_(defrecord Node [value branch1 branch2]
  Tree)

#_(ann-record [x :- Any] Node [value :- x
                             branch1 :- Tree
                             branch2 :- Tree])

;; (Node. 8 0 0)
