(ns customcoll.vector-test
  (:require [clojure.test :refer :all]
            [customcoll.vector :refer :all]))


(defcustomvec SquareVector [content]
  (nth [_ i] (let [n (get content i)]
               (* n n))))

(defn derefable? [x]
  (instance? clojure.lang.IDeref x))

(defcustomvec MyVec [contents]
  (nth [this i]
       (let [v (nth contents i)]
         (if (derefable? v) @v v)))
  (nth [this i not-found]
       (let [v (nth contents i not-found)]
         (if (derefable? v) @v v))))


(deftest TestBasicMethods
  (let [v (SquareVector. [1 2 3])]
    (testing "get"
      (is (= 1 (get v 0)))
      (is (= 4 (get v 1)))
      (is (= 9 (get v 2))))

    (testing "nth with 2 arguments"
      (is (= 1 (nth v 0)))
      (is (= 4 (nth v 1)))
      (is (= 9 (nth v 2))))

    (testing "nth with 3 arguments"
      (is (= 99 (nth v 5 99))))

    (testing "valAt with 2 arguments"
      (is (= 1 (.valAt v 0)))
      (is (= 4 (.valAt v 1)))
      (is (= 9 (.valAt v 2))))

    (testing "valAt with 3 arguments"
      (is (= 99 (.valAt v 5 99))))

    (testing "entryAt"
      (is (= 1 (.valAt v 0)))
      (is (= 4 (.valAt v 1)))
      (is (= 9 (.valAt v 2))))

    (testing "count"
      (is (= 3 (count v))))

    (testing "assoc and assocN"
      (let [v2 (assoc v 1 4)] (is (= 16 (nth v2 1))))
      (let [v2 (assoc v 3 4)] (is (= 16 (nth v2 3))))
      (let [v2 (.assoc v 3 4)] (is (= 16 (nth v2 3))))
      (let [v2 (.assocN v 3 4)] (is (= 16 (nth v2 3)))))

    (testing "seq and rseq"
      (is (= '(1 4 9) (seq v)))
      (is (= '(9 4 1) (rseq v))))
    ))

(deftest TestStdFuncs
  (let [v (SquareVector. [4 5 6])]
    (testing "map"
      (is (= [15 24 35]
             (map dec v))))
    (testing "filter"
      (is (= [16 36]
             (filter even? v))))))


(defcustomvec ShiftRangeVector [contents base]
  (nth [_ i]
       (.nth contents (- i base)))
  (nth [_ i not-found]
       (.nth contents (- i base) not-found))
  (assoc [_ i v]
         (ShiftRangeVector. (.assoc contents (- i base) v) base))
  (seq [this]
       (.seq contents))
  (rseq [this]
        (.rseq contents)))

(deftest TestNegativeRange
  (let [v (ShiftRangeVector. [1 2 3] -1)]
    (is (= 1 (nth v -1)))
    (is (= 2 (nth v 0)))
    (is (= 3 (nth v 1)))
    (let [v (assoc v -1 9)]
      (is (= '(9 2 3)
             v)))))



