(ns clojs.compiler-test
  (:require clojs.builtin.core)
  (:use clojure.test
        [clojure.contrib.string :only [substring?]]
        clojs.compiler))

(deftest clean-ids
  (testing "Testing clean IDs"
    (let [id (clean-id "hello-world!-+")]
      (is (not (substring? "-" id)))
      (is (not (substring? "+" id)))
      (is (not (substring? "!" id))))))

(deftest expr-test
  (testing "Expressions"
    (is (= (exp->js 10) [:num 10]))
    (is (= (exp->js "str") [:string "str"]))
    (is (= (exp->js 'a-sym) [:id "a$mn$sym"]))
    (is (= (exp->js :key) [:keyword "key"]))
    (is (= (exp->js [1 2]) [:vector  [[:num 1] [:num 2]]]))
    (is (= (exp->js {:name "Pete"}) [:map  [[[:keyword "name"] [:string "Pete"]]]]))
    (is (= (exp->js true) [:true]))
    (is (= (exp->js false) [:false]))
    (is (= (exp->js '(:name map)) [:methodcall [:id "map"] "get" [[:keyword "name"]]]))
    ))

(deftest builtins-test
  (testing "Builtins"
    (is (= (exp->js '(+ 1 2 3)) [:op '+ [:op '+ [:num 1] [:num 2]] [:num 3]]))
    (is (= (exp->js '(if true 1 2)) [:if [:true] [:num 1] [:num 2]]))
    (is (= (exp->js '(= 1 2)) [:op '=== [:num 1] [:num 2]]))
    (is (= (exp->js '(def a [1 2 3])) [:vardeclinit "a" [:vector [[:num 1] [:num 2] [:num 3]]]]))
    (is (= (exp->js '(do 1 2)) [:expblock [[:num 1] [:num 2]]]))
    (is (= (exp->js '(fn [a b] (+ a b))) [:function ["a" "b"] [[:return [:op '+ [:id "a"] [:id "b"]]]]]))
    (is (= (exp->js '(let [a 1
                           b 2]
                       (+ a b)))
           [:expblock [[:vardeclinit "b" [:num 2]]
                       [:vardeclinit "a" [:num 1]]
                       [:op '+ [:id "a"] [:id "b"]]]]))
    (is (= (exp->js '(str 1 " is " "big")) [:op '+ [:op '+ [:op '+ [:string ""] [:num 1]] [:string " is "]] [:string "big"]]))
    (is (= (exp->js '(set a 10)) [:assign "a" [:num 10]]))
    (is (= (exp->js '(log "Test")) [:call [:id "console.log"] [[:string "Test"]]]))
    ))
