(ns clojs.compiler
    (:gen-class)
    (:use clojs.rhino-js))

(defn- paren [e]
  (str "(" e ")"))

(declare sym-to-js vec-to-js list-to-js kw-to-js num-to-js str-to-js map-to-js)

(defn exp-to-js [e]
  ;(println "Exp: " e)
  (cond
    (symbol? e) (sym-to-js e)
    (number? e) (num-to-js e)
    (string? e) (str-to-js e)
    (keyword? e) (kw-to-js e)
    (vector? e) (vec-to-js e)
    (map?    e) (map-to-js e)
    (seq? e)   (list-to-js e)
    :else (throw (RuntimeException. (str "Could not translate: " e " of type: " (class e))))))

(defn stats-to-js [stats]
  (if (empty? stats)
    ""
    (str (exp-to-js (first stats)) "\n" (stats-to-js (rest stats)))))

(defn- comma-separate [lst]
  (if (empty? lst)
    ""
    (reduce (fn [e1 e2] (str e1 ", " e2)) lst)))

(defn str-to-js [e]
  "Naive implementation, does not concider escapes yet"
  (str "\"" e "\""))

(defn sym-to-js [e]
  (str e))

(defn num-to-js [e]
  (str e))

(defn kw-to-js [e]
  (str "'" (.substring (str e) 1) "'"))

(defn vec-to-js [e]
  (str "Array(" (comma-separate (map exp-to-js e)) ")"))

(defn map-to-js [e]
  (str "({" (comma-separate
             (map
               (fn [v] (str (exp-to-js (first v)) ": " (exp-to-js (second v))))
               e))
    "})"))


(defn- list-op [op js-op args]
  (paren (reduce (fn [e1 e2] (str e1 " " js-op " " e2)) (map exp-to-js args))))

(defmulti list-to-js first)

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
  (println (second lst))
  (str "(function(" (comma-separate (second lst)) ") {" (exps-to-js (rest (rest lst))) "})"))

(defmethod list-to-js 'clojure.core/fn [lst]
  (let [body (second lst)]
    (str "(function(" (comma-separate (first body)) ") {" (exps-to-js (rest body)) "})")))

(defn- lets-to-vars
  ([] "")
  ([b v & rest] (str "var " (sym-to-js b) " = " (exp-to-js v) "; " (apply lets-to-vars rest))))

(defmethod list-to-js 'let [lst]
  (let [[bindings & body] (rest lst)]
    (str "(function() { " (apply lets-to-vars bindings) (exps-to-js body) " })()")))

(defmethod list-to-js :default [lst]
  (cond
    (keyword? (first lst)) (str (exp-to-js (second lst)) "[" (kw-to-js (first lst)) "]")
    :else (let [expanded (macroexpand lst)]
        (if (= lst expanded)
            (str (first lst) "(" (reduce (fn [e1 e2] (str e1 ", " e2)) (map exp-to-js (rest lst))) ")")
            (do
            (println expanded)
            (list-to-js expanded))))))