(ns clojs.test
 (:gen-class)
 (:use clojs.compiler clojs.rhino-js))

(defn test-form [form]
  (with-js-scope
    (print "Test: " form)
    (let [clojure-result (eval form)
          js-result      (js-eval (exp-to-js form))]
      (if (= clojure-result js-result)
        (println "\t[OK]")
        (println "\t[FAIL] Got: " js-result " expected: " clojure-result " for JS: " (exp-to-js form))))))

(defn -main [& args]
  (test-form '3)
  (test-form '(+ 1 2 3))
  (test-form '(let [x 2
                    y 6] (* x y)))
  (test-form '(do (* 200 200)
                (- 100 100)))
  )