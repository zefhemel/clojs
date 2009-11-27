(ns clojs.builtin.core
 (:use clojs.compiler))

;; Numeric operators
(defbuiltin + [& lst]
  [:op '+ (map exp-to-js lst)])

(defbuiltin - [& lst]
  [:op '- (map exp-to-js lst)])

(defbuiltin * [& lst]
  [:op '* (map exp-to-js lst)])

(defbuiltin / [& lst]
  [:op '/ (map exp-to-js lst)])

(defbuiltin = [e1 e2]
  [:methodcall (exp-to-js e1) "equals" [(exp-to-js e2)]])

;; Special forms
(defbuiltin def [var value]
  [:vardeclinit (clean-id var) (exp-to-js value)])

(defn- exps-to-js [exps]
  (if (= (count exps) 1) ; last exp, return it!
    (do (println exps) (if (and (seq? (first exps))
                             (= (first (first exps)) 'set!)) ; no return before assignment
      (list (exp-to-js (first exps)))
      (list [:return (exp-to-js (first exps))])))
    (cons (exp-to-js (first exps)) (exps-to-js (rest exps)))))

(defbuiltin do [& lst]
  [:expblock (map exp-to-js lst)])

(defbuiltin fn [& lst]
  (let [body (first lst)]
    (if (vector? (first body)) ; multiple arity macro-expanded fn
      [:function (map exp-to-js (first body)) (exps-to-js (rest body))]
      [:function (map exp-to-js body) (exps-to-js (rest lst))])))

(defn- lets-to-vars
  ([] [])
  ([b v & rest] (conj (apply lets-to-vars rest) [:vardeclinit (clean-id b) (exp-to-js v)])))

(defbuiltin let [bindings & body]
  [:expblock (concat (apply lets-to-vars bindings) (map exp-to-js body))])

(defbuiltin println [& lst]
  [:call [:id "java.lang.System.out.println"] [(exp-to-js (cons 'str lst))]])

(defbuiltin str [& lst]
  [:op '+ (concat [[:string ""]] (map exp-to-js lst))])

(defbuiltin set! [var value]
  [:assign var (exp-to-js value)])

;; Seq operations

(defn- wrap-seq [e]
  [:methodcall e "seq" []])

(defbuiltin seq [e]
  (wrap-seq (exp-to-js e)))

(defn- build-list [lst]
  (if (= (count lst) 0)
    [:null]
    [:new "Cons" [(exp-to-js (first lst)) (build-list (rest lst))]]))

(defbuiltin list [& lst]
  (build-list lst))

(defbuiltin cons [hd tl]
  [:methodcall (wrap-seq (exp-to-js tl)) "cons" [(exp-to-js hd)]])

(defbuiltin first [lst]
  [:methodcall [:methodcall (exp-to-js lst) "seq" []] "first" []])

(defbuiltin second [lst]
  [:methodcall [:methodcall (wrap-seq (exp-to-js lst)) "rest" []] "first" []])

(defbuiltin rest [lst]
  [:methodcall [:methodcall (exp-to-js lst) "seq" []] "rest" []])

(defbuiltin map [fn lst]
  [:call [:id "map"] [(exp-to-js fn) (wrap-seq (exp-to-js lst))]])

;; Vector operations
;(defmethod list-to-js 'get [[_