(ns clojs.util
  (:gen-class)
  (:use clojs.compiler
        clojs.pp))

(def ^:dynamic *all-js-defs* (ref {}))

(defmacro js-defn  [name params & body]
  (if-not (vector? params)
    (throw (IllegalArgumentException. "second arguments should be a vector of argument names"))
    `(dosync
       (commute *all-js-defs* assoc (quote ~name)
                               {:language :javascript
                                :code (quote ~(apply list 'defn name params body))}))))

(defmacro js-def  [name value]
  `(dosync
       (commute *all-js-defs* assoc (quote ~name)
                               {:language :javascript
                                :code (quote ~(list 'def name value))})))

(defn all-js []
  (apply str (map 
               (comp (fn [v] (js-indent-semi v "")) exp-to-js :code)
               (vals @*all-js-defs*))))

(defn all-js-code []
  (map :code (vals @*all-js-defs*)))

(defn compile-file [in-filename out-filename]
  (let [reader (java.io.PushbackReader. (java.io.FileReader. in-filename))
        writer (java.io.PrintWriter. (java.io.FileOutputStream. out-filename))]
    (.println writer "load('js/clojs.js')")
    (loop [parsed (try-read reader)]
      (println "Just parsed: " parsed)
      (if-not (nil? parsed)
        (do
          (.println writer (js (exp-to-js parsed)))
          (recur (try-read reader)))))
    (.close reader)
    (.close writer)))
