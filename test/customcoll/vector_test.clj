(ns customcoll.vector-test
  (:require [clojure.test :refer :all]
            [customcoll.vector :refer :all]))

(defcustomvec SquareVector [content]
  (nth [_ i] (let [n (get content i)]
               (* n n))))

(deftest TestSquare
  (let [v (SquareVector. [1 2 3])]
    (testing "nth"
      (is (= 1 (nth v 0)))
      (is (= 4 (nth v 1)))
      (is (= 9 (nth v 2))))

    (testing "get"
      (is (= 1 (get v 0)))
      (is (= 4 (get v 1)))
      (is (= 9 (get v 2))))

    (testing "valAt"
      (is (= 1 (.valAt v 0)))
      (is (= 4 (.valAt v 1)))
      (is (= 9 (.valAt v 2))))
    
    (is (= 3 (count v)))
    (is (= 1 (get v 0)))
    (is (= 4 (get v 1)))
    (is (= 9 (get v 2)))
    (is (= '(1 4 9) (seq v)))
    (is (= '(9 4 1) (rseq v)))
    ))



