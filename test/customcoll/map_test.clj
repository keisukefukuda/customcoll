(ns customcoll.map-test
  (:require [clojure.test :refer :all]
            [customcoll.map :refer :all]))

(deftest TestBasicMethods
  (testing "Creation without options"
    (let [m (custom-map {1 2 3 4})]
      (is true)))


  (testing "Basic element access"
    (let [m (custom-map {1 2 3 4})]
      ;; seq and equality
      (is (= {1 2 3 4} m))
      (is (= (hash-map 1 2 3 4) m))
      (is (= m (hash-map 1 2 3 4)))
      (is (= (array-map 1 2 3 4) m))
      (is (= m (array-map 1 2 3 4)))
      (is (= [[1 2] [3 4]] (seq m)))
      (is (= (seq m) [[1 2] [3 4]]))
      
      ;; get/get-in
      (is (= 2 (m 1)))
      (is (= 2 (get m 1)))
      (is (= 2 (get m 1 :not-found)))
      (is (= 2 (get-in m [1])))
      (is (= 2 (get-in m [1] :not-found)))
      (is (nil? (m -1)))
      (is (nil? (get m -1)))
      (is (= 100 (get m -1 100)))

      ;; contains?
      (is (= true (contains? m 1)))
      (is (= false (contains? m -1)))

      ;; find
      (is (= [1 2] (find m 1)))
      (is (nil? (find m -1)))

      ;; keys
      (is (= [1 3] (keys m)))
      (is (= [2 4] (vals m)))

      ;; assoc
      (is (= 'foo ((assoc m 3 'foo) 3)))
      (is (= {1 2 3 99} (assoc m 3 99)))

      ;; reduce-kv
      (is (= 6 (reduce-kv (fn [acc k v] (+ acc v)) 0 m))) ;; reduce(+) on values
      (is (= 4 (reduce-kv (fn [acc k v] (+ acc k)) 0 m))) ;; reduce(+) on keys
      (is (= 10 (reduce-kv (fn [acc k v] (+ acc v k)) 0 m))) ;; reduce(+) on both
      ))
  
  (testing "misc map operations"
    ))


