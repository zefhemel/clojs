(ns clojs.rhino-js
  (:gen-class))

(declare *js-context* *js-scope*)

(defn with-js-scope* [fn]
  (binding [*js-context* (org.mozilla.javascript.Context/enter)]
    (binding [*js-scope*   (.initStandardObjects *js-context*)]
      (.evaluateReader *js-context* *js-scope* (java.io.FileReader. "js/clojs.js") "clojs.js" 1 nil)
      (fn)
      (org.mozilla.javascript.Context/exit))))

(defmacro with-js-scope [& body]
  `(with-js-scope* (fn [] ~@body)))

(defn js-eval [#^String s]
  (.evaluateString *js-context* *js-scope* s "<cmd>" 1 nil))
