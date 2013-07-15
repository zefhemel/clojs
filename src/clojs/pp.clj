(ns clojs.pp
  (:gen-class))

(defn comma-separate [lst]
  (if (empty? lst)
    ""
    (reduce (fn [e1 e2] (str e1 ", " e2)) lst)))

(defmulti pp-js
  (fn [vec indent] (first vec)))

(defn js [vec]
  (pp-js vec ""))

(defmacro defpp [type args body]
  `(defmethod pp-js ~type [[~(gensym) ~@args] ~(quote indent)]
     ~body))

(defn pp-js-semi [vec indent]
  (str (pp-js vec indent) ";\n"))

(defpp :string [s]
  (str indent "\"" s "\""))

(defpp :num [n]
  (str indent n))

(defpp :id [id]
  (str indent id))

(defpp :keyword [id]
  (str indent "\"" id "\""))

(defpp :null []
  (str indent "null"))

(defpp :op [op op1 op2]
  (str indent "(" (pp-js op1 indent) " " (str op) " " (pp-js op2 indent) ")"))

(defpp :call [fun-id args]
  (str indent (js fun-id) "(" (comma-separate (map js args)) ")"))

(defpp :vardeclinit [id value]
  (str indent "var " id " = " (js value)))

(defpp :return [value]
  (str indent "return " (js value)))

(defpp :assign [var value]
  (str indent var " = " (js value)))

(defpp :fieldaccess [exp field]
  (str indent (js exp) "." field))

(defpp :methodcall [exp name args]
  (let [ex (if (and (vector? exp) (= (first exp) :num))
             (str "(" (js exp) ")")
             (js exp))]
    (str indent ex "." name "(" (comma-separate (map js args)) ")")))

(defpp :new [cls args]
  (str indent "new " cls "(" (comma-separate (map js args)) ")"))

(defpp :map [values]
  (str indent "{" (comma-separate
                   (map
                    (fn [v] (str "\n" (pp-js (first v) (str "  " indent)) ": " (js (second v))))
                    values))
       "\n" indent "}"))

(defpp :vector [values]
  (str indent "[" (comma-separate (map js values)) "]"))

(defpp :function [args body]
  (str indent "(function(" (comma-separate (map js args)) ") {\n"
    (apply str (map pp-js-semi body (cycle [(str "  " indent)]))) "})"))

(defn- expblock-insert-return [exps]
  (if (= (count exps) 1) ; last exp, return it!
    [[:return (first exps)]]
    (cons (first exps) (expblock-insert-return (rest exps)) )))

(defpp :expblock [exps]
  (pp-js [:call [:function [] (expblock-insert-return exps)] []] indent))
