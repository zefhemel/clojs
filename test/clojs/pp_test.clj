(ns clojs.pp-test
  (:use clojure.test
        clojs.pp))

(deftest basics-test
  (testing "Basic expressions"
    (is (= (js [:num 10]) "10"))
    (is (= (js [:string "Pete"]) "\"Pete\""))
    (is (= (js [:id "name"])) "name")
    (is (= (js [:op '+ [:num 1] [:num 2]]) "(1 + 2)"))
    (is (= (js [:call [:id "add"] [[:num 1] [:num 2]]]) "add(1, 2)"))
    (is (= (js [:map [[ [:keyword "name"] [:string "Pete"]]]]) "{\n  \"name\": \"Pete\"\n}"))
    )
)
