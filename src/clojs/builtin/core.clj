(ns clojs.builtin.core
  (:use clojs.compiler))

;; Numeric operators
(defmethod list-to-js '+ [lst]
  (list-op '+ "+" (rest lst)))

(defmethod list-to-js '- [lst]
  (list-op '- "-" (rest lst)))

(defmethod list-to-js '* [lst]
  (list-op '* "*" (rest lst)))

(defmethod list-to-js '/ [lst]
  (list-op '/ "/" (rest lst)))

;; Special forms
(defmethod list-to-js 'def [lst]
  (str "var " (second lst) " = " (exp-to-js (nth lst 2)) ";\n"))

(defn- exps-to-js [exps]
  (if (= (count exps) 1) ; last exp, return it!
    (str "return " (exp-to-js (first exps)) ";")
    (apply str (exp-to-js (first exps)) "; " (exps-to-js (rest exps)))))

(defmethod list-to-js 'do [lst]
  (str "(function() { " (exps-to-js (rest lst)) "})()"))

(defmethod list-to-js 'fn [lst]
  (println lst)
  (let [body (second lst)]
      (if (vector? (first body)) ; multiple arity macro-expanded fn
        (str "(function(" (comma-separate (first body)) ") {" (exps-to-js (rest body)) "})")
        (str "(function(" (comma-separate body) ") {" (exps-to-js (rest (rest lst))) "})"))))

(defn- lets-to-vars
  ([] "")
  ([b v & rest] (str "var " (sym-to-js b) " = " (exp-to-js v) "; " (apply lets-to-vars rest))))

(defmethod list-to-js 'let [lst]
  (let [[bindings & body] (rest lst)]
    (str "(function() { " (apply lets-to-vars bindings) (exps-to-js body) " })()")))

(defmethod list-to-js 'println [lst]
  (str "java.lang.System.out.println('' + " (exp-to-js (second lst)) ")"))

(defmethod list-to-js 'str [lst]
  (str "'' + " (list-op '+ "+" (rest lst))))


;; List operations

(defn- build-list [lst]
  (if (= (count lst) 0)
    "null"
    (str "new Cons(" (exp-to-js (first lst)) ", " (build-list (rest lst)) ")")))

(defmethod list-to-js 'list [lst]
  (let [items (rest lst)]
    (str "(" (build-list items) ")")))

(defmethod list-to-js 'first [lst]
  (str "(" (exp-to-js (second lst)) ".head)"))

(defmethod list-to-js 'second [lst]
  (str "(" (exp-to-js (second lst)) ".tail.head)"))

(defmethod list-to-js 'rest [lst]
  (str "(" (exp-to-js (second lst)) ".tail)"))
