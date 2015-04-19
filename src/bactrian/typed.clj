(ns bactrian.typed
  (:require [clojure.core.typed :as t]
            [clojure.string :as s]))

(t/tc-ignore
 (defn gen-defs-named [type-name [name args]]
   (let [rec-name (-> name str s/capitalize symbol)
         sup-args (vec (concat [['++ :- 't/Kw]] args))
         arg-names (map first args)
         arg-types (map last args)
         any (symbol (str "Any" type-name))]
     `((t/ann-record ~rec-name
                     [~name :- (t/HMap :mandatory ~(zipmap (map keyword arg-names)
                                                           arg-types))])
       (defrecord ~rec-name [~name])
       (t/ann ~(vary-meta name assoc :no-check true)
              [ & :mandatory ~(zipmap (map keyword arg-names) arg-types) ~'-> ~any ])
       (defn ~name [ & {:keys [~@arg-names]} ]
         (new ~rec-name ~(zipmap (map keyword arg-names) arg-names))))))

 (defn gen-defs-pos [type-name [name args]]
   (let [rec-name (-> name str s/capitalize symbol)
         f-args (map #(symbol (str "a_" %2)) args (range))
         any (symbol (str "Any" type-name))]
     `((t/ann-record ~rec-name [~name :- (t/HVec [ ~@args ])])
       (defrecord ~rec-name [~name])
       (t/ann ~(vary-meta name assoc :no-check true) [ ~@args ~'-> ~any ])
       (defn ~name [ ~@f-args ] (new ~rec-name [~@f-args])))))

 (defn adt-named [type-name def-pairs]
   (let [defs (mapcat (partial gen-defs-named type-name)
                      def-pairs)
         types (mapcat (fn [[name args]]
                         (let [arg-names (map first args)
                               arg-types (map last args)]
                           `([~(keyword name) (t/HMap :mandatory ~(zipmap (map keyword arg-names)
                                                                          arg-types))])))
                       def-pairs)]
     `(do (t/defalias ~(symbol (str "Any" type-name))
            (t/HMap :mandatory ~(apply hash-map (apply concat types))))
          (t/defprotocol ~type-name)
          ~@defs)))

 (defn make-type-pairs [[rec-name rec-args]]
   [(-> rec-name name keyword) `(t/HVec ~rec-args)])

 (defn adt-pos [type-name def-pairs]
   (let [defs (mapcat (partial gen-defs-pos type-name)
                      def-pairs)]
     `(do (t/defprotocol ~type-name)
          (t/defalias ~(symbol (str "Any" type-name))
            (t/HMap :mandatory ~(apply hash-map (apply concat (map make-type-pairs def-pairs)))))
          ~@defs)))

 (defmacro defadt [type-name & body]
   (let [pairs (partition 2 body)]
     (if (->> (map second pairs)
              (apply concat)
              (filter keyword?)
              empty?)
       (adt-pos type-name pairs)
       (let [named-pairs (map (clojure.core/fn [[c-name c-args]]
                                [c-name (map vec (partition 3 c-args))])
                              pairs)]
         (adt-named type-name named-pairs))))))
