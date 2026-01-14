(ns lox.error)

;Error handling
(def hadError (atom false))

(defn report [line where message]
  (println (str "[line " line "] Error" where ": " message))
  (reset! hadError true)
)

;Helper function that will print out errors
(defn error [line message]
  (report line "" message)
)