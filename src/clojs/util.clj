(ns clojs.util
  (:gen-class)
  (:use clojs.compiler))

(def *all-js-defs* (ref {}))

(defmacro js-defn  [name params & body]
  (if-not (vector? params)
    (throw (IllegalArgumentException. "second arguments should be a vector of argument names"))
    `(dosync
       (commute *all-js-defs* assoc (quote ~name)
                               {:language :javascript
                                :code (quote ~(apply list 'defn name params body))}))))

(defmacro js-def  [name value]
  `(dosync
       (commute *all-js-defs* assoc (quote ~name)
                               {:language :javascript
                                :code (quote ~(list 'def name value))})))

(defn all-js []
  (apply str (map (comp exp-to-js :code) (vals @*all-js-defs*))))

(defn all-js-code []
  (map :code (vals @*all-js-defs*)))