(ns hiccup-framework7.def)

(defn wrap-component [f]
  (fn [& args]
    (if (map? (first args))
      (f (first args) (rest args))
      (f {} args))))

(defmacro defcomponent
  [name & fdecl]
  `(do (defn ~name ~@fdecl)
      (alter-var-root (var ~name) wrap-component)))
