(ns clojs.pp
  (:gen-class))

(defn comma-separate [lst]
  (if (empty? lst)
    ""
    (reduce (fn [e1 e2] (str e1 ", " e2)) lst)))

(defmulti js-indent (fn [vec indent] (first vec)))

(defn js [vec]
  (js-indent vec ""))

(defn js-indent-semi [vec indent]
  (str (js-indent vec indent) ";\n"))

(defmethod js-indent :string [[_ s] indent]
  (str indent "\"" s "\""))

(defmethod js-indent :num [[_ n] indent]
  (str indent n))

(defmethod js-indent :id [[_ id] indent]
  (str indent id))

(defmethod js-indent :null [_ indent]
  (str indent "null"))

(defmethod js-indent :op [[_ op operands] indent]
  (str indent "(" (reduce (fn [e1 e2] (str e1 " " op " " e2)) (map js-indent operands (cycle [""]))) ")"))

(defmethod js-indent :call [[_ fun-id args] indent]
  (str indent (js fun-id) "(" (comma-separate (map js args)) ")"))

(defmethod js-indent :vardeclinit [[_ id value] indent]
  (str indent "var " id " = " (js value)))

(defmethod js-indent :return [[_ value] indent]
  (str indent "return " (js value)))

(defmethod js-indent :assign [[_ var value] indent]
  (str indent var " = " (js value)))

(defmethod js-indent :fieldaccess [[_ exp field] indent]
  (str indent (js exp) "." field))

(defmethod js-indent :methodcall [[_ exp name args] indent]
  (let [ex (if (and (vector? exp) (= (first exp) :num))
             (str "(" (js exp) ")")
             (js exp))]
    (str indent ex "." name "(" (comma-separate (map js args)) ")")))

(defmethod js-indent :new [[_ cls args] indent]
  (str indent "new " cls "(" (comma-separate (map js args)) ")"))

(defmethod js-indent :map [[_ values] indent]
  (str indent "({" (comma-separate
             (map
               (fn [v] (str "\n" (js-indent (first v) (str "  " indent)) ": " (js (second v))))
               values))
    "\n" indent "})"))

(defmethod js-indent :vector [[_ values] indent]
  (str indent "new Vector([" (comma-separate (map js values)) "])"))

(defmethod js-indent :function [[_ args body] indent]
  (str indent "(function(" (comma-separate (map js args)) ") {\n"
    (apply str (map js-indent-semi body (cycle [(str "  " indent)]))) "})"))

(defn- expblock-insert-return [exps]
  (if (= (count exps) 1) ; last exp, return it!
    [[:return (first exps)]]
    (cons (first exps) (expblock-insert-return (rest exps)) )))

(defmethod js-indent :expblock [[_ exps] indent]
  (js-indent [:call [:function [] (expblock-insert-return exps)] []] indent))