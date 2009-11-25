(ns rhino-js
  (:gen-class))

(declare *js-context* *js-scope*)

(defn with-js-scope* [fn]
  (binding [*js-context* (org.mozilla.javascript.Context/enter)]
    (binding [*js-scope*   (.initStandardObjects *js-context*)]
        (fn))))

(defmacro with-js-scope [& body]
  `(with-js-scope* (fn [] ~@body)))

(defn js-eval [s]
  (.evaluateString *js-context* *js-scope* s "<cmd>" 1 nil))
