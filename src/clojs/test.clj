(ns clojs.test
 (:gen-class)
 (:use clojs.compiler
       clojs.builtin.core
       clojs.rhino-js
       clojs.util))

(defn test-form [form]
    (print "Test: " form)
    (let [clojure-result (eval form)
          js-result      (try
                           (js-eval (exp-to-js form))
                           (catch Exception e
                             (str "JS Result: " (exp-to-js form))))]
      (if (= clojure-result js-result)
        (println "\t[OK]")
        (println "\t[FAIL] Got: " js-result " expected: " clojure-result " for JS: " (exp-to-js form)))))

(defn js-run [form]
  (js-eval (exp-to-js form)))

(js-def my-name "Zef")

(js-defn test-fun [a b]
  (+ a b))

(js-defn say-hello [name]
  (+ "Hello, " name))

(defn -main [& args]
  (with-js-scope
    (test-form '3)
    (test-form '(+ 1 2 3))
    (test-form '(let [x 2
                      y 6] (* x y)))
    (test-form '(do (* 200 200)
                    (- 100 100)))
    (test-form '(:name {:name "Zef" :age 26}))
  )
  (println "--- Collected Javascript functions -----")
  (println (all-js))
  (println (all-js-code))
  (println "---- Some tests ----")
  (with-js-scope
    (js-run '(def my_name "Zef Hemel"))
    (js-run '(println (second (list 1 2 3))))
    )
  )

