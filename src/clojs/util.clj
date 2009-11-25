(ns clojs.util
  (:gen-class)
  (:use clojs.compiler))

(defmacro defnjs [name params & body]
  (if-not (vector? params)
    (throw (IllegalArgumentException. "second arguments should be a vector of argument names"))
    `(def ~name {:language :javascript :code ~(exp-to-js (apply list 'defn name params body))})))

(defn- all-js-maps [ns]
  (map (comp deref second)
    (filter (fn [[k v]]
              (let [val (deref v)]
                (and (map? val) (= (:language val) :javascript)))) (ns-publics ns))))

(defn all-js [ns]
  (apply str (map :code (all-js-maps ns))))

(defnjs test-fun [a b]
  (+ a b))

(defnjs say-hello [name]
  (+ "Hello, " name))

(defn -main [& args]
    (println (all-js 'clojs.util)))