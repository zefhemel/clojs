(ns clojs.compiler
  (:gen-class))

(declare
 sym-to-js
 vec-to-js
 list-to-js
 kw-to-js
 num-to-js
 str-to-js
 map-to-js)

(defn clean-id [id]
  (.replaceAll (str id) "[^a-zA-Z\\$]" "_"))

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
    []
    (conj (stats-to-js (rest stats))
          (exp-to-js (first stats)))))

(defn str-to-js [e]
  [:string e])

(defn sym-to-js [e]
  [:id (clean-id e)])

(defn num-to-js [e]
  [:num e])

(defn kw-to-js [e]
  [:string (.substring (clean-id e) 1)])

(defn vec-to-js [e]
  [:vector (map exp-to-js e)])

(defn map-to-js [e]
  [:map (map (fn [[k v]] [(exp-to-js k) (exp-to-js v)]) e)])

(defmulti list-to-js
  (fn [lst]
    (let [parts (.split (str (first lst)) "/")]
      (if (> (count parts) 1)
        (symbol (aget parts 1))
        (first lst)))))

(defmacro defbuiltin [fun args & body]
  `(defmethod list-to-js (quote ~fun) [[~(gensym) ~@args]]
     ~@body))

(defmethod list-to-js :default [lst]
  (cond
   (keyword? (first lst)) [:methodcall (exp-to-js (second lst)) "get" [(kw-to-js (first lst))]]
   :else (let [expanded (macroexpand lst)]
           (if (= lst expanded)
             [:call [:id (clean-id (first lst))] (map exp-to-js (rest lst))]
             (list-to-js expanded)))))

(defn try-read [reader]
  (try
    (read reader)
    (catch Exception e
      (if (= (.indexOf (.getMessage e) "EOF") -1)
        (throw (RuntimeException. e))
        nil))))
