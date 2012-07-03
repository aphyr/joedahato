(ns joedahato.core-test
  (:use clojure.test
        joedahato.core)
  (:import (joedahato.core Vector)))

(deftest dot-product-test
         (is (= 1 (dot-product [1] [1])))
         (is (= 0 (dot-product [-5 2] [2 5])))
         (is (= 1 (dot (Vector. [1]) [1])))
         (is (= 0 (dot (Vector. [-5 2]) [2 5]))))

(deftest scalar-product-test
         (is (= [0 0 0] (scalar-product 0 [1 2 0])))
         (is (= [-5 -10 0] (scalar-product 5 [-1 -2 0]))))

(deftest sum-test
         (is (= [0 -1 5] (sum [1 2 3] [-1 -3 2]))))
