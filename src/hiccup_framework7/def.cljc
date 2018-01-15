(ns hiccup-framework7.def
  (:require [clojure.string :as string]))

(defn classes [m]
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

(defn add-attr-map [hiccup]
  (let [[x & xs] hiccup]
    (if (map? (first xs))
      hiccup
      [x {} xs])))

(defn wrap-component [f]
  (fn [& args]
    (let [first-arg (first args)
          {:keys [attrs] :as props} (if (map? first-arg) first-arg {})
          result (f props (if (map? first-arg) (rest args) args))]
      (-> result
          add-attr-map
          ((partial clojure.walk/postwalk
                    #(cond-> % (associative? (:class %)) (update :class classes))))
          (update 1 merge-attrs attrs)))))

(defmacro defcomponent
  [name & fdecl]
  `(do (defn ~name ~@fdecl)
       (alter-var-root (var ~name) wrap-component)))
