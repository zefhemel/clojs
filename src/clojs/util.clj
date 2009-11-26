(ns clojs.util
  (:gen-class)
  (:use clojs.compiler))

(defmacro js-defn  [name params & body]
  (if-not (vector? params)
    (throw (IllegalArgumentException. "second arguments should be a vector of argument names"))
    `(def ~name {:language :javascript
                 :code (quote ~(apply list 'defn name params body))
                 :code-string ~(exp-to-js (apply list 'defn name params body))})))

(defmacro js-def  [name value]
  `(def ~name {:language :javascript
               :code (quote ~(list 'def name value))
               :code-string ~(exp-to-js (list 'def name value))}))

(defn- all-js-maps [ns]
  (map (comp deref second)
    (filter (fn [[k v]]
              (let [val (deref v)]
                (and (map? val) (= (:language val) :javascript)))) (ns-publics ns))))

(defn all-js [ns]
  (apply str (map :code-string (all-js-maps ns))))

(defn all-js-code [ns]
  (map :code (all-js-maps ns)))