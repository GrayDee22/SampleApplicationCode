(ns lox.interpreter
  (:require 
   [lox.expr :as expr]
   [lox.runtimeError :as runTime]
   [clojure.string :as str]
   [lox.stmt :as stmt]
   [lox.environment :as env]
   [lox.LoxFunction :refer [makeLoxFunction]]
   [lox.LoxClass :refer [makeLoxClass]] 
   ))


(defn evaluate [expression env visitor]
  (expr/accept expression env visitor)
)

(defn execute [stmt env visitor]
  (stmt/acceptStmt stmt env visitor)
)

(defn resolveInterpreter [resolver expr depth] 
  (let [key (if (map? expr) 
              (:lexeme expr) 
              ;else its the string of the expr
              (str expr))]
    (swap! (:locals (:interpreter resolver)) assoc key depth))
)


(defn lookUpVariable [interpreter env name expr]
  (let [distance (get @(:locals interpreter) expr)
        key      (if (map? name) (:lexeme name) name)
        value    (if distance
                   (env/getAt env distance key)
                   (env/getVar env key))] 
    value)
)

(declare Interpreter)

(defn executeBlock [statements newEnv]
  (loop [remaining statements] ;iterate over each statement in the block
    (when (seq remaining) ;when theres still new remaining stmts left
      (execute (first remaining) newEnv Interpreter)  ;execute each statement in the context of newEnv, passes Interpreter as needed for evaluation
      (recur (rest remaining))))
)

(defn isTruthy [object]
  (cond
    (= object nil) false
    (boolean? object) object
    :else true
    )
)

(defn isEqual [a b]
  (cond
    (and (= a nil) (= b nil)) true
    (= a nil) false 
    :else (= a b)
    )
)

(defn checkNumberOperand [operator operand]
  (if (number? operand)
    nil  ;it is a number so do nothing, is valid
    (runTime/runtimeError "Operand must be a number" :operator operator))
)

(defn checkNumberOperands [operator left right]
  (if (and (number? left) (number? right))
    nil  ;both are numbers so do nothing, is valid
    (runTime/runtimeError "Operands must be numbers"
                          :operator operator
                          :left left
                          :right right))
)

(defn stringify [object]
  (cond
    (nil? object) "nil"
    (true? object) "true"
    (false? object) "false"
    (number? object) (let [text (str object)]
                       (if (str/ends-with? text ".0") ;checks if text ends with ".0"
                         (subs text 0 (- (count text) 2))
                          ;else returns the text
                         text))
    :else (str object))
)


;interpreter visitor map with logic similar to AstPrinter and the book
(def Interpreter
  {:locals (atom {})
   :visitLiteral  (fn [_env expr] (:value expr))  ;literal's just return the value
   :visitGrouping (fn [env expr] (evaluate (:expression expr) env Interpreter)) ;evaluates the inner expr
   :visitUnary    (fn [env expr]
                    (let [right (evaluate (:right expr) env Interpreter)]
                      (case (:operator expr)
                        :- (do (checkNumberOperand (:operator expr) right)
                               (- right))
                        :!  (not (isTruthy right))
                        nil))) ;unreachable
   :visitBinary   (fn [env expr]
                    (let [left  (evaluate (:left expr) env Interpreter)
                          right (evaluate (:right expr) env Interpreter)] 
                      (case (:operator expr)
                        :+  (cond ;checks if it's adding a number or concatenating strings
                              (and (number? left) (number? right)) (+ left right)
                              (and (string? left) (string? right)) (str left right)
                              :else
                              (runTime/runtimeError
                               "Operands must be two numbers or two strings"
                               :left left :right right))
                        :- (do (checkNumberOperands (:operator expr) left right)
                               (- left right))
                        :*  (do (checkNumberOperands (:operator expr) left right)
                                (* left right))
                        :/ (do (checkNumberOperands (:operator expr) left right)
                               (/ left right))
                        :> (do (checkNumberOperands (:operator expr) left right)
                               (> left right))
                        :>= (do (checkNumberOperands (:operator expr) left right)
                                (>= left right))
                        :< (do (checkNumberOperands (:operator expr) left right)
                               (< left right))
                        :<= (do (checkNumberOperands (:operator expr) left right) 
                                (<= left right))
                        :!=  (not (isEqual left right))
                        :== (isEqual left right)
                        nil))) ;unreachable

   :visitVariableExpr (fn [env expr] (lookUpVariable Interpreter env (:name expr) expr))

   :visitAssignExpr (fn [env expr]
                     (let [value (evaluate (:value expr) env Interpreter)
                           distance (get @(:locals Interpreter) expr)]
                       (if distance
                         (env/assignAt env distance (:name expr) value)
                         (env/assign env (:name expr) value))
                       value))
  
   :visitBlockStmt (fn [env stmt] (let [blockEnv (env/makeEnvironment env)] 
                                   (executeBlock (:statements stmt) blockEnv) 
                                   nil))

   :visitClassStmt (fn [env stmt] (let [className (:lexeme (:name stmt))  
                                        _ (env/define env className nil)  

                                        superclass (when (:superclass stmt) ;if not superclass will be nil
                                                     (let [sc (evaluate (:superclass stmt) env Interpreter)] 
                                                       (when (not (and (map? sc) (:methods sc))) ; simple check for LoxClass map 
                                                         (runTime/runtimeError "Superclass must be a class" :token (:name (:superclass stmt)))) 
                                                       sc))
                                        ; if there is a superclass, create a temporary env for 'super' 
                                        classEnv (if superclass 
                                                   (let [tempEnv (env/makeEnvironment env)] 
                                                     (env/define tempEnv "super" superclass) 
                                                     tempEnv) ;returns the tempEnv
                                                   ;else returns the regular env
                                                   env) 
                                         
                                        methodsMap (loop [remainingMethods (:methods stmt) ; build a methods map 
                                                          result {}] 
                                                     (if (seq remainingMethods) 
                                                       (let [method      (first remainingMethods) 
                                                             isInit?     (= (:lexeme (:name method)) "init") 
                                                             function    (makeLoxFunction method classEnv executeBlock isInit?) ;only true for init 
                                                             methodName  (:lexeme (:name method))] 
                                                         (recur (rest remainingMethods) (assoc result methodName function)))
                                                       ;else returns the result
                                                       result)) 
                                        
                                        klass (makeLoxClass className superclass methodsMap)
                                        
                                        ; restores the environment if we used a temp one for 'super'
                                        finalEnv (if superclass 
                                                   (:enclosing classEnv) ; returns the original env  
                                                   ;else returns the current one
                                                   classEnv)] 

                                        (env/assign finalEnv className klass) 
                                        nil))

   
   :visitExpressionStmt (fn [env stmt] (evaluate (:expression stmt) env Interpreter))

   :visitPrintStmt (fn [env stmt] (let [value (evaluate (:expression stmt) env Interpreter)] 
                                   (println (if (and (map? value) (:toString value)) 
                                              ((:toString value))  ; for classes
                                              (stringify value)))  ; everything else 
                                   nil))
  
   :visitVarStmt (fn [env stmt] (let [value (when (:initializer stmt) 
                                             (evaluate (:initializer stmt) env Interpreter))] 
                                 (env/define env (:name stmt) value)  ; pass value to define 
                                 nil))
   
   :visitFunctionStmt (fn [env stmt] (let [closure (loop [current env]  ; capture the currnet environment 
                                                     (if (:enclosing current) 
                                                       (if (:isFunctionScope current) 
                                                         current
                                                         ;else 
                                                         (recur (:enclosing current)))
                                                       ;else 
                                                       current)) 
                                           function (makeLoxFunction stmt closure executeBlock false)]
                                       ; define function in current environment 
                                       (env/define env (:lexeme (:name stmt)) function)) 
                        nil)
  
   :visitReturnStmt (fn [env stmt] (let [value (if (not (nil? (:value stmt)))
                                                (evaluate (:value stmt) env Interpreter)
                                                   ;else
                                                nil)]
                                    (throw (ex-info "Return" {:value value})))) 

   :visitIfStmt (fn [env stmt] (if (isTruthy (evaluate (:condition stmt) env Interpreter)) 
                                 (execute (:thenBranch stmt) env Interpreter) 
                                 ;else
                                 (when (:elseBranch stmt) 
                                   (execute (:elseBranch stmt) env Interpreter))
                                 )
                  nil) ;returns nil/null
   :visitLogicalExpr (fn [env expr] (let [left (evaluate (:left expr) env Interpreter)]
                                       (if (= (:type (:operator expr)) :OR)
                                         (if (isTruthy left)
                                           left
                                           ;else
                                           (evaluate (:right expr) env Interpreter)) ; else evaluate right
                                          ; AND case
                                         (if (not (isTruthy left))
                                           left
                                           ;else
                                           (evaluate (:right expr) env Interpreter)))))
   
   :visitSetExpr (fn [env expr] (let [object (evaluate (:object expr) env Interpreter)] 
                                  (if (and (map? object) (:set object))  ; checks if its a map because LoxInstance is a map 
                                    (let [value (evaluate (:value expr) env Interpreter)] 
                                      ((:set object) object (:name expr) value) 
                                      value) 
                                    ;else
                                    (throw (ex-info "Only instances have fields." {:token (:name expr)})))))
   
   :visitSuperExpr (fn [env expr] (let [key (:lexeme (:super expr)) ; use the lexeme of the 'super' token 
                                        
                                        distance (get @(:locals Interpreter) key)

                                        superclass (env/getAt env distance "super") 

                                        object (env/getAt env (dec distance) "this") 

                                        method ((:findMethod superclass) superclass (:lexeme (:method expr)))] 
                                    (when (nil? method) 
                                      (runTime/runtimeError (str "Undefined property '" (:lexeme (:method expr)) "'.") :token (:method expr))) 
                                    ((:bind method) method object)))

   :visitThisExpr (fn [env expr] (lookUpVariable Interpreter env (:keyword expr) expr))
   
   :visitWhileStmt (fn [env stmt] (loop []
                                     (when (isTruthy (evaluate (:condition stmt) env Interpreter))
                                       (execute (:body stmt) env Interpreter)
                                       (recur)))
                                   nil) ;returns nil/null  
   
   :visitCallExpr (fn [env expr] (let [callee (evaluate (:callee expr) env Interpreter) 
                                       arguments (loop [args [] remaining (:arguments expr)] 
                                                   (if (seq remaining) 
                                                     (recur (conj args (evaluate (first remaining) env Interpreter)) (rest remaining)) 
                                                     ;else return the args
                                                     args))]
                                   ; type check, callee must have :call and :arity 
                                   (when (or (not (map? callee)) 
                                             (not (contains? callee :call)) 
                                             (not (contains? callee :arity))) 
                                     (runTime/runtimeError "Can only call functions and classes." :token (:paren expr)))
                                   ; max arguments check 
                                   (when (>= (count arguments) 255) 
                                     (runTime/runtimeError "Can't have more than 255 arguments." :token (:paren expr)))  
                                   (let [expected ((:arity callee) callee)] 
                                     (when (not= (count arguments) expected) 
                                       (runTime/runtimeError (str "Expected " expected " arguments but got " (count arguments) ".") 
                                                             :token (:paren expr))))
                                   ; call the function 
                                   ((:call callee) callee Interpreter arguments)))
   
   :visitGetExpr (fn [env expr] (let [object (evaluate (:object expr) env Interpreter)] 
                                  (if (and (map? object) (:get object)) ; checks if its a map because LoxInstance is a map
                                    ((:get object) object (:name expr)) ; call the :get function 
                                    ;else
                                    (throw (ex-info "Only instances have properties." {:token (:name expr)})))))
   }
)


(defn interpret [stmts env]
  (try
    (loop [remaining stmts]
      (when (seq remaining)
        (execute (first remaining) env Interpreter)
        (recur (rest remaining))))
    (catch clojure.lang.ExceptionInfo e
      (runTime/runtimeError (ex-message e) :exception e)))
)