(ns clj-puzzles.dst.bst-test
  (:require [clojure.test :refer :all]
            [clj-puzzles.dst.bst :refer :all :as bst]))

(deftest test-insert
  (let [tree (reduce bst/insert (bst/insert 10) [5 15 12 17 3 6])]
    (testing "BST insert nodes"
      (is (= (:d tree) 10) "Root is first ele inserted")
      (is (= (get-in tree [:r :d]) 15) "insert right")
      (is (= (get-in tree [:l :d]) 5) "insert left")
      (is (= (get-in tree [:r :l :d]) 12) "insert rl")
      (is (= (get-in tree [:r :r :d]) 17) "insert rr")
      (is (= (get-in tree [:l :l :d]) 3) "insert ll")
      (is (= (get-in tree [:l :r :d]) 6) "insert lr"))

    (testing "BST delete nodes"
      (is (= nil (bst/delete (bst/insert 10) 10)) "del only element in tree")
      (let [del-root (bst/delete tree 10)]
        (is 
          (and 
            (= 12 (:d del-root)) 
            (= nil (get-in del-root [:r :l :d])))
          "delete with two children - replace w rsubtree")))))
