; NOTE: Although this file looks like clojure, it is in fact CloJS

(def records [])

(defn add-record [name age]
  (set! records (cons {:name name :age age} records)))

(add-record "Zef" 26)
(add-record "Danny" 25)

(println (map (fn [v] (:name v)) records))