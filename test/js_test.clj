; NOTE: Although this file looks like clojure, it is in fact CloJS

(def hits 0)

(defn say-hello [name]
  (set! hits (+ hits 1))
  (str "Hello, " name))

(defn say-hi [name]
  (set! hits (+ hits 1))
  (str "Hi, " (say-hello name)))

(println (say-hello "Zef"))
(println (say-hi "Zef"))
(println hits)