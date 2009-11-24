(ns clojs
    (:gen-class))

(defn- paren [e]
  (str "(" e ")"))

(def sym-to-js)
(def vec-to-js)
(def list-to-js)
(def kw-to-js)
(def num-to-js)
(def str-to-js)
(def map-to-js)

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

(defn str-to-js [e]
  "Naive implementation, does not concider escapes yet"
  (str "\"" e "\""))

(defn sym-to-js [e]
  e)

(defn num-to-js [e]
  e)

(defn kw-to-js [e]
  (str "'" (.substring (str e) 1) "'"))

(defn vec-to-js [e]
  (str "Array(" (reduce #(str %1 ", " %2) (map exp-to-js e)) ")"))

(defn map-to-js [e]
  (str "{" (reduce #(str %1 ", " %2) 
             (map
               (fn [v] (str (exp-to-js (first v)) ": " (exp-to-js (second v))))
               e))
    "}"))


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

(defn- comma-separate [lst]
  (if (empty? lst)
    ""
    (reduce (fn [e1 e2] (str e1 ", " e2)) lst)))

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

(defmethod list-to-js :default [lst]
  (let [expanded (macroexpand lst)]
    (if (= lst expanded)
        (str (first lst) "(" (reduce (fn [e1 e2] (str e1 ", " e2)) (map exp-to-js (rest lst))) ")")
        (do
          (println expanded)
          (list-to-js expanded)))))

(defn -main [& args]
  (println (exp-to-js '(defn dosomething [] (print "Hello!")))))