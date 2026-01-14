(ns lox.expr
  (:require [lox.runtimeError :as runTime])
)

; Expression types with their fields
(def exprTypes
  {:LITERAL  [:value]
   :GROUPING [:expression]
   :UNARY    [:operator :right] 
   :BINARY   [:left :operator :right]
   :VARIABLE [:name] 
   :ASSIGN   [:name :value]
   :LOGICAL  [:left :operator :right]
   :CALL     [:callee :paren :arguments]
   :GET      [:object :name]
   :SET      [:object :name :value]
   :THIS     [:keyword]
   :SUPER    [:keyword :method]
   }
)

(defn makeExpr [exprType & args]     ; grabs the expression type, and the & args gets however many fields there are
  (let [fields (exprTypes exprType)] ; grabs the fields based off the type of expr
    (if (= (count fields) (count args)) ; checks if the amount of args entered are = to the expr fields
    ; if they are, build the map manually using assoc repeatedly, pass the intial values
      (loop [m {:type exprType}
             f fields
             a args]
        (if (empty? f)             ; Checks if the fields are empty
          m                        ; if so we're done & return the map
        ; else add the first values to the map, and then recur the loop again
          (recur (assoc m (first f) (first a)) ;assoc adds the the first field and value pair to the map
                                   ; rest passes the next loop everything after the car of the list
                 (rest f) 
                 (rest a)))) 
          ; if the args and fields are not equal, prints an error message and reurns nil
          (do
           (println "Wrong number of args for" exprType " expected " (count fields) " but got " (count args)) 
            nil)
      ))
)

(defn visit [visitor env expr]
  ;(println "visitStmt type:" (:type expr))
  (cond
    (= :LITERAL (:type expr))   ((:visitLiteral visitor) env expr)
    (= :GROUPING (:type expr))  ((:visitGrouping visitor) env expr)
    (= :UNARY (:type expr))     ((:visitUnary visitor) env expr)
    (= :BINARY (:type expr))    ((:visitBinary visitor) env expr)
    (= :VARIABLE (:type expr))  ((:visitVariableExpr visitor) env expr)
    (= :ASSIGN (:type expr))    ((:visitAssignExpr visitor) env expr)
    (= :LOGICAL (:type expr))   ((:visitLogicalExpr visitor) env expr)
    (= :CALL (:type expr))      ((:visitCallExpr visitor) env expr)
    (= :GET (:type expr))       ((:visitGetExpr visitor) env expr)
    (= :SET (:type expr))       ((:visitSetExpr visitor) env expr)
    (= :THIS (:type expr))      ((:visitThisExpr visitor) env expr)
    (= :SUPER (:type expr))     ((:visitSuperExpr visitor) env expr)
    :else (runTime/runtimeError "Unknown expression type in visit" :expr expr))
)

(defn accept [expr env visitor]
  (visit visitor env expr)
)

;prints out the Ast trees of the expressions
;does it recursively by calling accept with the expr and AstPrinter for each type of expr so it can handle nested expressions
;; (def AstPrinter
;;   ;a visitor map, containing functions names on the left and there body on the right
;;   {:visitLiteral  (fn [expr] (str (:value expr)))
;;    :visitGrouping (fn [expr] (str "(group " (accept (:expression expr) AstPrinter) ")"))
;;    :visitUnary    (fn [expr] (str "(" (:operator expr) " " (accept (:right expr) AstPrinter) ")"))
;;    :visitBinary   (fn [expr] (str "(" (accept (:left expr) AstPrinter) " "
;;                                   (:operator expr) " "
;;                                   (accept (:right expr) AstPrinter) ")"))}
;; )  
  