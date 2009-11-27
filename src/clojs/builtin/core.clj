(ns clojs.builtin.core
  (:use clojs.compiler))

;; Numeric operators
(defmethod list-to-js '+ [[_ & lst]]
  [:op '+ (map exp-to-js lst)])

(defmethod list-to-js '- [[_ & lst]]
  [:op '- (map exp-to-js lst)])

(defmethod list-to-js '* [[_ & lst]]
  [:op '* (map exp-to-js lst)])

(defmethod list-to-js '/ [[_ & lst]]
  [:op '/ (map exp-to-js lst)])

(defmethod list-to-js '= [[_ e1 e2]]
  [:methodcall (exp-to-js e1) "equals" [(exp-to-js e2)]])

;; Special forms
(defmethod list-to-js 'def [lst]
  [:vardeclinit (clean-id (second lst)) (exp-to-js (nth lst 2))])

(defn- exps-to-js [exps]
  (if (= (count exps) 1) ; last exp, return it!
    (list [:return (exp-to-js (first exps))])
    (cons (exp-to-js (first exps)) (exps-to-js (rest exps)))))

(defmethod list-to-js 'do [lst]
  [:expblock (map exp-to-js (rest lst))])

(defmethod list-to-js 'fn [lst]
  (let [body (second lst)]
      (if (vector? (first body)) ; multiple arity macro-expanded fn
        [:function (map exp-to-js (first body)) (exps-to-js (rest body))]
        [:function body (exps-to-js (rest (rest lst)))])))

(defn- lets-to-vars
  ([] [])
  ([b v & rest] (conj (apply lets-to-vars rest) [:vardeclinit (clean-id b) (exp-to-js v)])))

(defmethod list-to-js 'let [lst]
  (let [[bindings & body] (rest lst)]
    [:expblock (concat (apply lets-to-vars bindings) (map exp-to-js body))]))

(defmethod list-to-js 'println [lst]
  [:call [:id "java.lang.System.out.println"] [[:op '+ [[:string ""] (exp-to-js (second lst))]]]])

(defmethod list-to-js 'str [lst]
  [:op '+ (concat [[:string ""]] (map exp-to-js (rest lst)))])

(defmethod list-to-js 'set! [[_ var value]]
  [:assign var (exp-to-js value)])

;; List operations

(defn- build-list [lst]
  (if (= (count lst) 0)
    [:null]
    [:new "Cons" [(exp-to-js (first lst)) (build-list (rest lst))]]))

(defmethod list-to-js 'list [lst]
  (let [items (rest lst)]
    (build-list items)))

(defmethod list-to-js 'first [lst]
  [:methodcall [:methodcall (exp-to-js (second lst)) "seq" []] "first" []])

(defmethod list-to-js 'second [lst]
  [:methodcall [:methodcall [:methodcall (exp-to-js (second lst)) "seq" []] "rest" []] "first" []])

(defmethod list-to-js 'rest [lst]
  [:methodcall [:methodcall (exp-to-js (second lst)) "seq" []] "rest" []])
