(ns clojs.compiler
  (:require [clojure.string :as string])
  (:gen-class))

(declare
 sym->js
 vec->js
 list->js
 kw->js
 num->js
 str->js
 bool->js
 map->js)

(defn clean-id [id]
  (-> (str id)
      (string/replace "-" "$mn$")
      (string/replace "+" "$pl$")
      (string/replace "!" "$ex$")      
      (.replaceAll "[^a-zA-Z\\$]" "_")))

(defn exp->js [e]
  (cond
   (symbol? e)  (sym->js e)
   (number? e)  (num->js e)
   (string? e)  (str->js e)
   (keyword? e) (kw->js e)
   (vector? e)  (vec->js e)
   (map? e)     (map->js e)
   (seq? e)     (list->js e)
   (or (true? e) (false? e)) (bool->js e)
   :else (throw
          (RuntimeException. (str "Could not translate: " e " of type: " (class e))))))

(defn stats->js [stats]
  (if (empty? stats)
    []
    (conj (stats->js (rest stats))
          (exp->js (first stats)))))

(defn str->js [e]
  [:string e])

(defn sym->js [e]
  [:id (clean-id e)])

(defn num->js [e]
  [:num e])

(defn kw->js [e]
  [:keyword (.substring (str e) 1)])

(defn vec->js [e]
  [:vector (map exp->js e)])

(defn map->js [e]
  [:map (map (fn [[k v]] [(exp->js k) (exp->js v)]) e)])

(defn bool->js [e]
  (if (true? e)
    [:true]
    [:false]))

(defmulti list->js
  (fn [lst]
    (let [parts (.split (str (first lst)) "/")]
      (if (> (count parts) 1)
        (symbol (aget parts 1))
        (first lst)))))

(defmacro defbuiltin [fun args & body]
  `(defmethod list->js (quote ~fun) [[~(gensym) ~@args]]
     ~@body))

(defmethod list->js :default [lst]
  (cond
   (keyword? (first lst)) [:methodcall (exp->js (second lst)) "get" [(kw->js (first lst))]]
   :else (let [expanded (macroexpand lst)]
           (if (= lst expanded)
             [:call [:id (clean-id (first lst))] (map exp->js (rest lst))]
             (list->js expanded)))))

(defn try-read [reader]
  (try
    (read reader)
    (catch Exception e
      (if (= (.indexOf (.getMessage e) "EOF") -1)
        (throw (RuntimeException. e))
        nil))))
