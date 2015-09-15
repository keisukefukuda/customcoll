(ns customcoll.map-test
  (:require [clojure.test :refer :all]
            [customcoll.map :refer :all]))

(deftest TestBasicMethods
  (testing "Creation without options"
    (let [m (custom-map {1 2 3 4})]
      (is true)))

  (let [m (custom-map {1 2 3 4})]
    
    ;; seq and equality
    (is (= m m))
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

    (is (= [1 3] (map key m)))
    (is (= [2 4] (map val m)))

    ;; get-in, assoc-in for nested data maps
    (let [m2 {:mymap m :foo :bar}]
      (is (= 2 (get-in m2 [:mymap 1])))
      (is (= {:mymap {1 2 3 4 5 6} :foo :bar}
             (assoc-in m2 [:mymap 5] 6)))
      (is (= {:mymap {1 2 3 6} :foo :bar}
             (assoc-in m2 [:mymap 3] 6))))

    ;; dissoc
    (is (= m (dissoc m)))
    (is (= {1 2} (dissoc m 3)))
    (is (= {} (dissoc m 1 3)))
    (is (= m (dissoc m 9 10 11 12)))

    ;; update
    (letfn [(square [x] (if x (* x x) 0))]
      (is (= {1 2 3 16} (update m 3 square)))
      (is (= {1 2 3 16} (update-in m [3] square)))
      (is (= {1 2 3 4 9 0} (update m 9 square)))
      (is (= {1 2 3 4 9 0} (update-in m [9] square))))

    ;; select-keys
    (is (= {1 2} (select-keys m [1])))
    (is (= {1 2 3 4} (select-keys m [1 3])))
    (is (= {1 2} (select-keys m [1 9])))
    
    ;; merge
    (is (= {1 2 3 4 5 6} (merge m {5 6})))
    (is (= {1 2 3 4 5 6} (merge {5 6} m)))
    (is (= {1 2 3 0} (merge m {3 0})))
    (is (= {1 2 3 4} (merge {3 0} m)))

    ;; merge-with
    (is (= {1 2 3 4 5 6} (merge-with + m {5 6})))
    (is (= {1 2 3 9} (merge-with + m {3 5})))))

(defn x2 [x] (* 2 x))

(deftest TestValueHook
  (testing "Simple value-hook function"
    (let [m (custom-map {1 2 3 4} :value-hook x2)]
    
      ;; seq and equality
      (is (= m m))
      (is (= {1 4 3 8} m))
      (is (= (hash-map 1 4 3 8) m))
      (is (= m (hash-map 1 4 3 8)))
      (is (= (array-map 1 4 3 8) m))
      (is (= m (array-map 1 4 3 8)))
      (is (= [[1 4] [3 8]] (seq m)))
      (is (= (seq m) [[1 4] [3 8]]))
      
      ;; get/get-in
      (is (= 4 (m 1)))
      (is (= 4 (get m 1)))
      (is (= 4 (get m 1 :not-found)))
      (is (= 4 (get-in m [1])))
      (is (= 4 (get-in m [1] :not-found)))
      (is (nil? (m -1)))
      (is (nil? (get m -1)))
      (is (= 100 (get m -1 100)))
      
      ;; contains?
      (is (= true (contains? m 1)))
      (is (= false (contains? m -1)))

      ;; find
      (is (= [1 4] (find m 1)))
      (is (nil? (find m -1)))

      ;; keys
      (is (= [1 3] (keys m)))
      (is (= [4 8] (vals m)))

      ;; assoc
      (is (= 9 ((assoc m 3 9) 3)))
      (is (= {1 4 3 20} (assoc m 3 20)))

      ;; reduce-kv
      (is (= 12 (reduce-kv (fn [acc k v] (+ acc v)) 0 m))) ;; reduce(+) on values
      (is (= 4 (reduce-kv (fn [acc k v] (+ acc k)) 0 m))) ;; reduce(+) on keys
      (is (= 16 (reduce-kv (fn [acc k v] (+ acc v k)) 0 m))) ;; reduce(+) on both

      (is (= [1 3] (map key m)))
      (is (= [4 8] (map val m)))

      ;; get-in, assoc-in for nested data maps
      (let [m2 {:mymap m :foo :bar}]
        (is (= 4 (get-in m2 [:mymap 1])))
        (is (= {:mymap {1 4 3 8 5 6} :foo :bar}
               (assoc-in m2 [:mymap 5] 6)))
        (is (= {:mymap {1 4 3 6} :foo :bar}
               (assoc-in m2 [:mymap 3] 6))))

      ;; dissoc
      (is (= m (dissoc m)))
      (is (= {1 4} (dissoc m 3)))
      (is (= {} (dissoc m 1 3)))
      (is (= m (dissoc m 9 10 11 12)))

      ;; update
      (letfn [(square [x] (if x (* x x) 0))]
        (is (= {1 4 3 64} (update m 3 square)))
        (is (= {1 4 3 64} (update-in m [3] square)))
        (is (= {1 4 3 8 9 0} (update m 9 square)))
        (is (= {1 4 3 8 9 0} (update-in m [9] square))))

      ;; select-keys
      (is (= {1 4} (select-keys m [1])))
      (is (= {1 4 3 8} (select-keys m [1 3])))
      (is (= {1 4} (select-keys m [1 9])))
      
      ;; merge
      (is (= {1 4 3 8 5 6} (merge m {5 6})))
      (is (= {1 4 3 8 5 6} (merge {5 6} m))) ;; In this case, 
      (is (= {1 4 3 0} (merge m {3 0})))
      (is (= {1 4 3 8} (merge {3 0} m)))

      ;; merge-with
      (is (= {1 4 3 8 5 6} (merge-with + m {5 6})))
      (is (= {1 4 3 13} (merge-with + m {3 5}))))))

