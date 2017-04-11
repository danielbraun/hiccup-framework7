(ns hiccup-framework7.components-test
  (:require [clojure.test :refer :all]
            [hiccup-framework7.components]))

(defn- valid-component? [f]
  (let [res (f {} "test")]
    (and (vector? res)
         (keyword? (first res)))))

(deftest component-test
  (testing "every public fn is a component and returns valid hiccup"
    (is (->> (ns-publics 'hiccup-framework7.components)
             vals
             (every? valid-component?)))))
