(ns hiccup-framework7.components-test
  (:require [clojure.test :refer :all]
            [clojure.string :as string]
            [hiccup-framework7.components]))

(defn- valid-component? [f]
  (let [id "some-id"
        res (f {:attrs {:id id}} "test")]
    (and (vector? res)
         (keyword? (first res))
         (string/includes? (get-in res [1 :id]) id))))

(deftest component-test
  (testing "every public fn is a component and returns valid hiccup"
    (is (->> (ns-publics 'hiccup-framework7.components)
             vals
             (every? valid-component?)))))
