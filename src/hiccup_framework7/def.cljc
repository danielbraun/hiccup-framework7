(ns hiccup-framework7.def
  (:require [clojure.string :as string]))

(defn- classes [m]
  (->> m
       (filter second)
       (map (comp #(cond->> %
                     (not (vector? %)) vector
                     true (remove nil?)
                     true (map (fn [x]
                                 (try (name x)
                                      (catch Exception e x))))
                     true (string/join "-"))
                  first))
       (string/join " ")))

(def ^:private merge-attrs
  (partial merge-with
           (comp (partial string/join " ")
                 vector)))

(defn wrap-component [f]
  (fn [& args]
    (->>
      (if (map? (first args))
        (let [props (first args)
              [el & content :as result] (f props (rest args))]
          (-> (if (map? (first content))
                result
                [el {} content])
              (update 1 merge-attrs (:attrs props))))
        (f {} args))
      (clojure.walk/postwalk
        #(cond-> % (associative? (:class %)) (update :class classes))))))

(defmacro defcomponent
  [name & fdecl]
  `(do (defn ~name ~@fdecl)
       (alter-var-root (var ~name) wrap-component)))
