(ns lox.stmt
  (:require [lox.runtimeError :as runTime])
)

; statement types with their fields
(def stmtTypes
  {:EXPRESSION [:expression]
   :PRINT      [:expression]
   :VAR        [:name :initializer]
   :BLOCK      [:statements]
   :IF         [:condition :thenBranch :elseBranch]
   :WHILE      [:condition :body] 
   :FUNCTION   [:name :params :body]
   :RETURN     [:keyword :value]
   :CLASS      [:name :superclass :methods]
   }
)

; Creates a statement with the same logic from expr.clj
(defn makeStmt [stmtType & args]
  (let [fields (stmtTypes stmtType)]
    (if (= (count fields) (count args))
      (loop [m {:type stmtType} 
             f fields 
             a args]
        (if (empty? f)
          m
          (recur (assoc m (first f) (first a))
                 (rest f)
                 (rest a))))
      (do
        (println "Wrong number of args for" stmtType)
        nil))))


; Visit function for statements
(defn visitStmt [visitor env stmt]
  ;(println "visitStmt type:" (:type stmt)) ; for debugging
  (cond
    (= :EXPRESSION (:type stmt)) ((:visitExpressionStmt visitor) env stmt)
    (= :PRINT (:type stmt))      ((:visitPrintStmt visitor) env stmt)
    (= :VAR (:type stmt))        ((:visitVarStmt visitor) env stmt)
    (= :BLOCK (:type stmt))      ((:visitBlockStmt visitor) env stmt)
    (= :IF (:type stmt))         ((:visitIfStmt visitor) env stmt)
    (= :WHILE (:type stmt))      ((:visitWhileStmt visitor) env stmt)
    (= :FUNCTION (:type stmt))   ((:visitFunctionStmt visitor) env stmt)
    (= :RETURN (:type stmt))     ((:visitReturnStmt visitor) env stmt)
    (= :CLASS (:type stmt))      ((:visitClassStmt visitor) env stmt)
    :else (runTime/runtimeError "Unknown statement type" :stmt stmt))
)

(defn acceptStmt [stmt env visitor]
  (visitStmt visitor env stmt)
)
