(ns lox.runtimeError)

(def hadRuntimeError (atom false)) ;global variable in clojure

(defn runtimeError [message & {:as info}] ;takes in the error message, and the remaning args as a key-value pair and stores them in a map called info
  (println "Runtime error:" message)
  (reset! hadRuntimeError true) ; marks that a runtime error occurred and sets it to true
  (throw (ex-info message (assoc info :type :runtimeError))) ;gives it the type of runtimeError
)