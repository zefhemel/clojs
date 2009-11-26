(ns clojs.compiler
    (:gen-class))
    
(defn- paren [e]
  (str "(" e ")"))

(declare sym-to-js vec-to-js list-to-js kw-to-js num-to-js str-to-js map-to-js)

(defn exp-to-js [e]
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

(defn comma-separate [lst]
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

(defn list-op [op js-op args]
  (paren (reduce (fn [e1 e2] (str e1 " " js-op " " e2)) (map exp-to-js args))))

(defmulti list-to-js
  (fn [lst]
    (let [parts (.split (str (first lst)) "/")]
      (if (> (count parts) 1)
        (symbol (aget parts 1))
        (first lst)))))

(defmethod list-to-js :default [lst]
  (cond
    (keyword? (first lst)) (str (exp-to-js (second lst)) "[" (kw-to-js (first lst)) "]")
    :else (let [expanded (macroexpand lst)]
        (if (= lst expanded)
            (str (first lst) "(" (reduce (fn [e1 e2] (str e1 ", " e2)) (map exp-to-js (rest lst))) ")")
            (list-to-js expanded)))))