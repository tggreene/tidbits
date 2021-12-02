(ns tggreene.tidbits.core-test
  (:require [clojure.test :refer [deftest testing is]]
            [tggreene.tidbits.core :as core]))

(deftest split-whenever
  (testing "doesn't split identical items"
    (is (= [[1 1 1 1 1]]
           (core/split-whenever identity (repeat 5 1)))))

  (testing "doesn't split identical f result"
    (is (= [[0 1 2 3 4]]
           (core/split-whenever int? (range 5)))))

  (testing "splits on f result change"
    (is (= [[0] [1] [2] [3] [4]]
           (core/split-whenever even? (range 5))))))

(deftest safe-div
  (testing "nil values"
    (is (nil? (core/safe-div nil nil)))
    (is (nil? (core/safe-div 1 nil)))
    (is (nil? (core/safe-div nil 1))))

  (testing "zero denominator"
    (is (nil? (core/safe-div 1 0))))

  (testing "non-numeric values"
    (is (nil? (core/safe-div true false)))
    (is (nil? (core/safe-div "1" "3")))
    (is (nil? (core/safe-div 1 "3"))))

  (testing "otherwise just div"
    (is (= (/ 1 3) (core/safe-div 1 3)))))
