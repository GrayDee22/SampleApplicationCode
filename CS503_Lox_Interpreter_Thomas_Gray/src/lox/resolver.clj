(ns lox.resolver
  (:require
   [lox.environment :as env]
   [lox.interpreter :as interpreter]
   [lox.expr :as expr]
   [lox.stmt :as stmt]
   [lox.error :refer [error]]))

; FunctionType enum
(def FunctionType
  {:NONE     :none
   :FUNCTION :function
   :METHOD   :method
   :INITIALIZER :initializer
   }
)

;ClassType enum
(def ClassType
  {:NONE :none
   :CLASS :class
   :SUBCLASS :subclass
})

; Resolver constructor equivalent
(defn resolver [interpreter]
  {:interpreter interpreter
   :scopes      (atom [])    ; stack of block scopes
   :env         env/env     ; stores the current environment 
   :currentFunction (atom (:NONE FunctionType)) ; tracks if we’re inside a function 
   :currentClass (atom (:NONE ClassType))
   }
)

(declare resolveExpr resolveStmt resolveStatements)

; swap! lets me update the value safely within an atom, 
; which for this file will allow me to make changes the resolver map safely


; push a new scope onto the stack
(defn beginScope [resolver parentEnv]
  (let [newEnv (env/makeEnvironment parentEnv)]
    (swap! (:scopes resolver) conj newEnv)
    newEnv) ; return it for use
)

; pop the current scope off the stack
(defn endScope [resolver]
  (swap! (:scopes resolver) pop)
)

; helper function to update the top scope
(defn put [resolver key value]
  ; update the top scope with key -> value
  (swap! (:scopes resolver)
         (fn [scopes]
           (if (seq scopes)
             (conj (pop scopes) (assoc (peek scopes) key value))
             ;else
             scopes)))
)

(defn declareResolver [resolver name]
  (when (seq @(:scopes resolver))
    (let [lex (if (map? name) (:lexeme name) (str name))
          scope (peek @(:scopes resolver))]
      (when (contains? scope lex)
        (error name "Already a variable with this name in this scope."))
      (put resolver lex false)))  ; false = declared but not yet defined
)


(defn define [resolver name]
  (when (seq @(:scopes resolver))
    (let [lex (if (map? name) (:lexeme name) (str name))]
      (put resolver lex true)))  ; true = fully defined
)

(defn resolveLocal [resolver expr name]
  (let [scopes @(:scopes resolver)
        size   (count scopes)
        lexeme (if (map? name) (:lexeme name) (str name))]
    (loop [i (dec size)]
      (when (>= i 0)
        (let [scope (nth scopes i)]
          (if (contains? scope lexeme)
              (interpreter/resolveInterpreter resolver expr (- (dec size) i))
            (recur (dec i)))))))
)

(defn resolveFunction [resolver function type]
  (let [enclosingFunction @(:currentFunction resolver)] ; save previous function type
    (swap! (:currentFunction resolver) (constantly type))
    
    (beginScope resolver (:env resolver))  ; begin a new scope just for parameters & function body

    ; declare & define parameters
    (doseq [param (:params function)]
      (declareResolver resolver param)
      (define resolver param))
    
    (resolveStatements resolver (:body function)) ; resolve the function body in this scope
    
    (endScope resolver) ; pop function scope
    
    (swap! (:currentFunction resolver) (constantly enclosingFunction)))
)

(defn visitBlockStmt [resolver stmt]
  (let [_blockEnv (beginScope resolver (:env resolver))] ; pass current env
    (resolveStatements resolver (:statements stmt))      ; resolve all statements in the block
    (endScope resolver))  ; discard the block scope                             
  nil
)

(defn visitClassStmt [resolver stmt]
  ; saves the previous class type and marks that it is inside a class
  (let [enclosingClass @(:currentClass resolver)]
    (reset! (:currentClass resolver) (:CLASS ClassType)) 
    
    (declareResolver resolver (:name stmt))
    (define resolver (:name stmt))

    (when (and 
           (not (nil? (:superclass stmt))) 
           (= (:lexeme (:name stmt)) (:lexeme (:name (:superclass stmt))))
           )
      (error (:name (:superclass stmt)) "A class can't inherit from itself.")) 
    
    (when (not (nil? (:superclass stmt)))
      (reset! (:currentClass resolver) (:SUBCLASS ClassType))
      (resolveExpr resolver (:superclass stmt)) 
      (beginScope resolver (last @(:scopes resolver))) 
      (define resolver "super"))
    
    (beginScope resolver (last @(:scopes resolver))) ; push the "this" scope
    (define resolver "this")

    (loop [methods (:methods stmt)]
      (when (seq methods) 
        (let [method (first methods) 
              fnType (if (= (:lexeme (:name method)) "init")
                         (:INITIALIZER FunctionType)
                       ;else
                         (:METHOD FunctionType))]
            (resolveFunction resolver method fnType))
          (recur (rest methods))))
    
    (endScope resolver)  ; pop the "this" scope

    (when (not (nil? (:superclass stmt)))
      (endScope resolver))
    
    (reset! (:currentClass resolver) enclosingClass) ; restore the previous class type

    nil)
)

(defn visitExpressionStmt [resolver stmt]
  (resolveExpr resolver (:expression stmt))
  nil
)

(defn visitFunctionStmt [resolver stmt]
  (declareResolver resolver (:name stmt))
  (define resolver (:name stmt))
  (resolveFunction resolver stmt (:FUNCTION FunctionType))
  nil
)

(defn visitIfStmt [resolver stmt]
  (resolveExpr resolver (:condition stmt))
  (resolveStmt resolver (:thenBranch stmt))
  (when (:elseBranch stmt)
    (resolveStmt resolver (:elseBranch stmt)))
  nil
)

(defn visitPrintStmt [resolver stmt]
  (resolveExpr resolver (:expression stmt))
  nil
)

(defn visitReturnStmt [resolver stmt]
; error handling
  (when (= @(:currentFunction resolver) (:NONE FunctionType)) 
    (error (:keyword stmt) "Can't return from top-level code.")) 
  
  (when (and (:value stmt) (= @(:currentFunction resolver) (:INITIALIZER FunctionType))) 
    (error (:keyword stmt) "Can't return a value from an initializer."))

  ; if no error resolve the return stmt
  (when (:value stmt)
    (resolveExpr resolver (:value stmt)))
  nil
)

(defn visitWhileStmt [resolver stmt]
  (resolveExpr resolver (:condition stmt))
  (resolveStmt resolver (:body stmt))
  nil
)

(defn visitBinaryExpr [resolver expr]
  (resolveExpr resolver (:left expr))
  (resolveExpr resolver (:right expr))
  nil
)

(defn visitCallExpr [resolver expr]
  (resolveExpr resolver (:callee expr))
  (loop [arguments (:arguments expr)]
    (when (seq arguments)  ; while there are still arguments
      (resolveExpr resolver (first arguments))
      (recur (rest arguments)))) ;loop with the rest of the arguments
  nil
)

(defn visitGetExpr [resolver expr]
  (resolveExpr resolver (:object expr))
  nil
)

(defn visitGroupingExpr [resolver expr]
  (resolveExpr resolver (:expression expr))
  nil
)

(defn visitLiteralExpr [_resolver _expr] ;just returns nil, not need for params
  nil
)

(defn visitLogicalExpr [resolver expr]
  (resolveExpr resolver (:left expr))
  (resolveExpr resolver (:right expr))
  nil
)

(defn visitSetExpr [resolver expr]
  (resolveExpr resolver (:value expr))
  (resolveExpr resolver (:object expr))
  nil
)

(defn visitSuperExpr [resolver expr]
  (cond
    (= @(:currentClass resolver) (:NONE ClassType))
                       (error (:keyword expr) "Can't use 'super' outside of a class.")
  
    (not (= @(:currentClass resolver) (:SUBCLASS ClassType))) 
                       (error (:keyword expr) "Can't use 'super' in a class with no superclass.")
    )
  
  (resolveLocal resolver expr (:keyword expr))
  nil
)

(defn visitThisExpr [resolver expr]
  (when (= @(:currentClass resolver) :none)
    (error (:keyword expr) "Can't use 'this' outside of a class."))
  (resolveLocal resolver expr (:keyword expr))
  nil
)

(defn visitUnaryExpr [resolver expr]
  (resolveExpr resolver (:right expr))
  nil
)

(defn visitVarStmt [resolver stmt]
  (declareResolver resolver (:name stmt))  ; declare first
  (when (:initializer stmt)                ; resolve initializer if it exists
    (resolveExpr resolver (:initializer stmt)))
  (define resolver (:name stmt))           ; always define
  nil
)

(defn visitAssignExpr [resolver expr]
  (resolveExpr resolver (:value expr))
  (resolveLocal resolver expr (:name expr))
  nil
)

(defn visitVariableExpr [resolver expr]
  (when (and 
         (seq @(:scopes resolver)) ;checks if its empty or not, = to (not (empty?))
         (= false (get (peek @(:scopes resolver)) (:lexeme (:name expr))))
         )
    (error (:name expr) "Can't read local variable in its own initializer."))
  ;else
  (resolveLocal resolver expr (:name expr))
  nil
)

(def ResolverVisitor
  {:visitExpressionStmt visitExpressionStmt
   :visitPrintStmt      visitPrintStmt
   :visitVarStmt        visitVarStmt
   :visitBlockStmt      visitBlockStmt
   :visitIfStmt         visitIfStmt
   :visitWhileStmt      visitWhileStmt
   :visitFunctionStmt   visitFunctionStmt
   :visitReturnStmt     visitReturnStmt
   :visitClassStmt      visitClassStmt

   :visitLiteral        visitLiteralExpr
   :visitGrouping       visitGroupingExpr
   :visitUnary          visitUnaryExpr
   :visitBinary         visitBinaryExpr
   :visitVariableExpr   visitVariableExpr
   :visitAssignExpr     visitAssignExpr
   :visitLogicalExpr    visitLogicalExpr
   :visitCallExpr       visitCallExpr 
   :visitGetExpr        visitGetExpr
   :visitSetExpr        visitSetExpr
   :visitThisExpr       visitThisExpr
   :visitSuperExpr      visitSuperExpr
   })

; Resolve a single statement; print 1 + 2;
(defn resolveStmt [resolver stmt]
  (stmt/acceptStmt stmt resolver ResolverVisitor) ; pass current environment for env & resolver as visitor
)

; Resolve a list of statements
;var a = 2;
;print a;
(defn resolveStatements [resolver statements]
  (loop [remaining statements]
    (when (seq remaining) ;if there are still remaining statements resolve them
      (resolveStmt resolver (first remaining))
      (recur (rest remaining))))
)

; Resolve a single expression; 1 + 2;
(defn resolveExpr [resolver expr]
  (expr/accept expr resolver ResolverVisitor) ; pass current environment for env & resolver as visitor
)