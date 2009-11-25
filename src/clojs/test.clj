(ns clojs.test
 (:gen-class)
 (:use clojs.compiler 
       clojs.rhino-js
       clojs.util))

(defn test-form [form]
    (print "Test: " form)
    (let [clojure-result (eval form)
          js-result      (try
                           (js-eval (exp-to-js form))
                           (catch Exception e
                             (str "JS Result: " (stats-to-js form))))]
      (if (= clojure-result js-result)
        (println "\t[OK]")
        (println "\t[FAIL] Got: " js-result " expected: " clojure-result " for JS: " (stats-to-js form)))))

;(defnjs test-fun [a b]
;  (+ a b))

(comment
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
  (println (filter (fn [[k v]] (= k 'test-fun)) (ns-publics *ns*)))
  )
)