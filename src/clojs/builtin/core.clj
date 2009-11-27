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

;; Special forms
(defmethod list-to-js 'def [lst]
  [:vardeclinit (second lst) (exp-to-js (nth lst 2))])

(defn- exps-to-js [exps]
  (if (= (count exps) 1) ; last exp, return it!
    [[:return (exp-to-js (first exps))]]
    (conj (exps-to-js (rest exps)) (exp-to-js (first exps)))))

(defmethod list-to-js 'do [lst]
  [:expblock (map exp-to-js (rest lst))])

(defmethod list-to-js 'fn [lst]
  (let [body (second lst)]
      (if (vector? (first body)) ; multiple arity macro-expanded fn
        [:function (map exp-to-js (first body)) (exps-to-js (rest body))]
        [:function body (exps-to-js (rest (rest lst)))])))

(defn- lets-to-vars
  ([] [])
  ([b v & rest] (conj (apply lets-to-vars rest) [:vardeclinit b (exp-to-js v)])))

(defmethod list-to-js 'let [lst]
  (let [[bindings & body] (rest lst)]
    [:expblock (concat (apply lets-to-vars bindings) (map exp-to-js body))]))

(defmethod list-to-js 'println [lst]
  [:call [:id "java.lang.System.out.println"] [[:op '+ [[:string ""] (exp-to-js (second lst))]]]])

(defmethod list-to-js 'str [lst]
  [:op '+ (concat [[:string ""]] (map exp-to-js (rest lst)))])

;; List operations

(defn- build-list [lst]
  (if (= (count lst) 0)
    [:null]
    [:new "Cons" [(exp-to-js (first lst)) (build-list (rest lst))]]))

(defmethod list-to-js 'list [lst]
  (let [items (rest lst)]
    (build-list items)))

(defmethod list-to-js 'first [lst]
  [:fieldaccess (exp-to-js (second lst)) "head"])

(defmethod list-to-js 'second [lst]
  [:fieldaccess [:fieldaccess (exp-to-js (second lst)) "tail"] "head"])

(defmethod list-to-js 'rest [lst]
  [:fieldaccess (exp-to-js (second lst)) "tail"])
