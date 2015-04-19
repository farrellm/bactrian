(ns bactrian.core)

(defn gen-defs [[name args]]
  (if (number? args)
    (let [arg-names (map #(symbol (str "a" %))
                         (range args))]
      `(defn ~name [ ~@arg-names ]
         [~(keyword name) ~@arg-names]))
    (if (empty? args)
      `(defn ~name []
         [~(keyword name)])
      (let [arg-names (map #(symbol (str "a__" %))
                           args)
            arg-pairs (map #(vector (keyword %1) %2)
                           args arg-names)]
        `(defn ~name [ ~@arg-names ]
           [~(keyword name) ~(into {} arg-pairs)])))))

(defn defadt* [type-name body]
  (let [defs (map gen-defs (partition 2 body))]
    `(do ~@defs)))

(defmacro defadt [name & body]
  (defadt* name body))
