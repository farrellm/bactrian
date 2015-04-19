(ns bactrian.typed
  {:squiggly {:checkers [:eastwood :typed]}}
  (:require [clojure.core.typed :as t]
            [clojure.string :as s]))

(t/defalias TypedName (t/HVec [t/Sym t/Kw t/Sym]))
(t/defalias NamedCons (t/HVec [t/Sym (t/List TypedName)]))

(t/defalias PositionalCons (t/HVec [t/Sym (t/List t/Sym)]))

(t/ann ^:no-check gen-defs-named [t/Sym NamedCons -> (t/List t/Any)])
(defn gen-defs-named [type-name [name args]]
  (let [rec-name (-> name str s/capitalize symbol)
        arg-names (map (t/fn [[n _ _] :- TypedName] n) args)
        arg-types (map (t/fn [[_ _ t] :- TypedName] t) args)
        any (symbol (str "Any" type-name))
        type-map (zipmap (map keyword arg-names) arg-types)]
    `((t/ann-record ~rec-name [~name :- (t/HMap :mandatory ~type-map)])
      (defrecord ~rec-name [~name])
      (t/ann ~(vary-meta name assoc :no-check true)
             [ & :mandatory ~(zipmap (map keyword arg-names) arg-types) ~'-> ~any ])
      (defn ~name [ & {:keys [~@arg-names]} ]
        (new ~rec-name ~(zipmap (map keyword arg-names) arg-names))))))

(t/ann ^:no-check adt-named [t/Sym (t/List NamedCons) -> (t/List t/Any)])
(defn adt-named [type-name def-pairs]
  (let [defs (mapcat (partial gen-defs-named type-name) def-pairs)
        types (mapcat (t/fn [[name args] :- NamedCons]
                        (let [arg-names (map first args)
                              arg-types (map last args)
                              type-map (zipmap (map keyword arg-names) arg-types)]
                          `([~(keyword name) (clojure.core.typed/HMap :mandatory ~type-map)])))
                      def-pairs)]
    `(do (t/defalias ~(symbol (str "Any" type-name))
           (t/HMap :mandatory ~(apply hash-map (apply concat types))))
         (t/defprotocol ~type-name)
         ~@defs)))

(t/ann ^:no-check gen-defs-pos [t/Sym PositionalCons -> (t/List t/Any)])
(defn gen-defs-pos [type-name [name args]]
  (let [rec-name (-> name str s/capitalize symbol)
        f-args (map #(symbol (str "a_" %2)) args (range))
        any (symbol (str "Any" type-name))]
    `((t/ann-record ~rec-name [~name :- (t/HVec [ ~@args ])])
      (defrecord ~rec-name [~name])
      (t/ann ~(vary-meta name assoc :no-check true) [ ~@args ~'-> ~any ])
      (defn ~name [ ~@f-args ] (new ~rec-name [~@f-args])))))

(t/ann make-type-pairs ['[t/Sym (t/List t/Sym)] -> '[t/Kw (t/List t/Any)]])
(defn make-type-pairs [[rec-name rec-args]]
  [(-> rec-name name keyword) (list 'clojure.core.typed/HVec rec-args)])

(t/ann ^:no-check adt-pos [t/Sym (t/List PositionalCons) -> (t/List t/Any)])
(defn adt-pos [type-name def-pairs]
  (let [defs (mapcat (partial gen-defs-pos type-name)
                     def-pairs)
        type-map (apply hash-map (mapcat make-type-pairs def-pairs))]
    `(do (t/defprotocol ~type-name)
         (t/defalias ~(symbol (str "Any" type-name))
           (t/HMap :mandatory ~type-map))
         ~@defs)))

(defmacro defadt [type-name & body]
  (let [pairs (partition 2 body)]
    (if (->> (map second pairs)
             (apply concat)
             (filter keyword?)
             empty?)
      (adt-pos type-name pairs)
      (let [named-pairs (map (fn [[c-name c-args]]
                               [c-name (map vec (partition 3 c-args))])
                             pairs)]
        (adt-named type-name named-pairs)))))
