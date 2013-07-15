(ns clojs.builtin.core
 (:use clojs.compiler))

;; Numeric operators
(defn- op-red [op args]
  (reduce (fn [a b] [:op op a b]) (map exp->js args)))

(defbuiltin + [& lst]
  (op-red '+ lst))

(defbuiltin - [& lst]
  (op-red '- lst))

(defbuiltin * [& lst]
  (op-red '* lst))

(defbuiltin / [& lst]
  (op-red '/ lst))

(defbuiltin = [e1 e2]
  [:op '=== (exp->js e1) (exp->js e2)])

;; Special forms
(defbuiltin def [var value]
  [:vardeclinit (clean-id var) (exp->js value)])

(defn- exps->js [exps]
  (if (= (count exps) 1) ; last exp, return it!
    (if (and (seq? (first exps))
             (= (first (first exps)) 'set)) ; no return before assignment
      (list (exp->js (first exps)))
      (list [:return (exp->js (first exps))]))
    (cons (exp->js (first exps)) (exps->js (rest exps)))))

(defbuiltin do [& lst]
  [:expblock (map exp->js lst)])

(defbuiltin fn [args & body]
  [:function (map clean-id args) (exps->js body)])

(defn- lets-to-vars
  ([] [])
  ([b v & rest] (conj (apply lets-to-vars rest) [:vardeclinit (clean-id b) (exp->js v)])))

(defbuiltin let [bindings & body]
  [:expblock (concat (apply lets-to-vars bindings) (map exp->js body))])

(defbuiltin log [& lst]
  [:call [:id "console.log"] (map exp->js lst)])

(defbuiltin str [& lst]
  (reduce (fn [a b] [:op '+ a b]) (concat [[:string ""]] (map exp->js lst))))

(defbuiltin set [var value]
  [:assign (str var) (exp->js value)])

(defbuiltin if [& lst]
  (if (= (count lst) 2)
    (let [[cond if-true] lst]
      [:if (exp->js cond) (exp->js if-true)])
    (let [[cond if-true if-false] lst]
      [:if (exp->js cond) (exp->js if-true) (exp->js if-false)])))


