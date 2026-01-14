(ns lox.parser
  (:require
   [lox.expr :as expr]
   [lox.error :refer [error]]
   [lox.stmt :as stmt]))

(def tokenOperator
  {:PLUS           :+
   :MINUS          :-
   :STAR           :*
   :SLASH          :/
   :GREATER        :>
   :GREATER_EQUAL  :>=
   :LESS           :<
   :LESS_EQUAL     :<=
   :BANG           :!
   :BANG_EQUAL     :!=
   :EQUAL_EQUAL    :==
   }
)
(defn makeParser [tokens]
  {
   :tokens tokens ;list of tokens passed into parser
   :current 0 ;current index
  }
)

(defn peekParser [parserMap]
  (let [tokens (:tokens parserMap)
        idx (:current parserMap)
        eof {:type :EOF :lexeme "" :literal nil :line 0}]
    (if (and tokens (< idx (count tokens)))
      (nth tokens idx)
      ;else
      eof)))

(defn previous [parserMap]
  (let [tokens (:tokens parserMap)
        idx (dec (:current parserMap))
        eof {:type :EOF :lexeme "" :literal nil :line 0}]
    (if (and tokens (>= idx 0) (< idx (count tokens)))
      (nth tokens idx)
      ;else
      eof)))


(defn isAtEnd [parserMap]
  (= (:type (peekParser parserMap)) :EOF))


(defn advanceParser [parserMap]
  (if (not (isAtEnd parserMap))
    (let [updated (update parserMap :current inc)]
      [updated (previous updated)]) 
    [parserMap (peekParser parserMap)]))


; checks if current token matches the passed in type
(defn check [parserMap tokenType]
  (let [token (peekParser parserMap)]
    (if token
      (= (:type token) tokenType)
      false)))

; checks if the types of different tokens match
(defn matchParser [parserMap & tokenTypes] ;&tokenTypes to be able to grab various lengths of types passed in
  (loop [types tokenTypes
         p parserMap] 
    (cond
      (empty? types) [false p]                          ; no match
      (check p (first types))                           ; matched
      (let [[newParserMap _] (advanceParser p)]         ; consume token
        [true newParserMap])                            ; returns true and the updated parserMap
      
      :else (recur (rest types) p))
    )
)

; forward declarations of expression and statement
(declare expression)
(declare statement)
(declare declaration)

(defn parseError [message token]
  (ex-info message {:token token :type :parseError})
)

(defn parse [parserMap]
  (loop [p parserMap
         stmts []] ;empty vector to hold the statements
    (if (isAtEnd p)
      stmts ;if at the end of the parserMap, just return the current statements vector
      (let [[stmt updatedParserMap] (declaration p)] ;grabs the new statement and updated parserMap
        (recur updatedParserMap (conj stmts stmt))))) ;adds the new read in statement to the vector of statements
)

(defn synchronize [parserMap]
  (let [[_ newMap] (advanceParser parserMap)] ; advance map
    (loop [p newMap]
      (if (isAtEnd p)
        p
        ;else
        (let [prev (previous p)
              current (peekParser p)
              tokenType (:type current)]
          (cond
            ; if the previous token was a semicolon, return current the parserMap
            (= (:type prev) :SEMICOLON) p

            ;switch style
            (= tokenType :CLASS) p
            (= tokenType :FUN)   p
            (= tokenType :VAR)   p
            (= tokenType :FOR)   p
            (= tokenType :IF)    p
            (= tokenType :WHILE) p
            (= tokenType :PRINT) p
            (= tokenType :RETURN) p

            ; else, keep looping
            :else (let [[_ nextParserMap] (advanceParser p)]
                    (recur nextParserMap)))))))
)

(defn consume [parserMap tokenType message]
  (let [[updated token] (advanceParser parserMap)]
    (if (= (:type token) tokenType)
      [token updated]
      ;else
      (throw (parseError message {:token (peekParser parserMap)}))))
)


(defn primary [parserMap]
  (let [[matchedFalse pF]     (matchParser parserMap :FALSE)
        [matchedTrue  pT]     (matchParser parserMap :TRUE)
        [matchedNil   pN]     (matchParser parserMap :NIL)
        [matchedLit pLit]     (matchParser parserMap :NUMBER :STRING)
        [matchedLP   pLP]     (matchParser parserMap :LEFT_PAREN)
        [matchedId   pId]     (matchParser parserMap :IDENTIFIER)
        [matchedThis pThis]   (matchParser parserMap :THIS)
        [matchedSuper pSuper] (matchParser parserMap :SUPER)] 

    (cond
      matchedFalse [(expr/makeExpr :LITERAL false) pF]
      matchedTrue  [(expr/makeExpr :LITERAL true) pT]
      matchedNil   [(expr/makeExpr :LITERAL nil) pN]
      matchedLit   [(expr/makeExpr :LITERAL (:literal (previous pLit))) pLit]

      matchedLP    (let [[innerExpr pInner] (expression pLP) 
                         [_ pRP] (consume pInner :RIGHT_PAREN "Expect ')' after expression.")] 
                     [(expr/makeExpr :GROUPING innerExpr) pRP]) 
      
      matchedId [(expr/makeExpr :VARIABLE (previous pId)) pId]

      matchedSuper (let [keyword (previous pSuper)
                         [_ p] (consume pSuper :DOT "Expect '.' after 'super'.")
                         [method p1] (consume p :IDENTIFIER  "Expect superclass method name.")]
                     [(expr/makeExpr :SUPER keyword method) p1])

      matchedThis [(expr/makeExpr :THIS (previous pThis)) pThis]
      
         :else
         (throw (parseError "Expect expression." {:token (peekParser parserMap)}))))
)

(defn finishCall [parserMap callee]
  (loop [arguments []
         p parserMap]
    (if (check p :RIGHT_PAREN)
      (let [[parenToken pFinal] (consume p :RIGHT_PAREN "Expect ')' after arguments.")]
        [(expr/makeExpr :CALL callee parenToken arguments) pFinal])
      ; else
      (let [[arg p1] (expression p)
            _ (when (>= (count arguments) 255)
                (error (peekParser p1) "Can't have more than 255 arguments."))
            [matchedComma p2] (matchParser p1 :COMMA)]
        (if matchedComma
          (recur (conj arguments arg) p2) ; keep collecting arguments
          ; last argument, no comma
          (let [[parenToken pFinal] (consume p2 :RIGHT_PAREN "Expect ')' after arguments.")]
            [(expr/makeExpr :CALL callee parenToken (conj arguments arg)) pFinal])))))
)

(defn call [parserMap]
  (let [[expr newMap] (primary parserMap)]
    (loop [expr expr
           p newMap]
      (let [[matchedLP p1] (matchParser p :LEFT_PAREN)]
        (if matchedLP 
          (let [[newExpr p2] (finishCall p1 expr)]
            (recur newExpr p2))
          ; else if
          (let [[matchedDot p2] (matchParser p :DOT)]
            (if matchedDot 
              (let [[name p3] (consume p2 :IDENTIFIER "Expect property name after '.'.")
                    expr      (expr/makeExpr :GET expr name)]
                (recur expr p3))
              ; else return the expr
              [expr p]))))))
)

(defn unary [parserMap]
  (let [[matched p] (matchParser parserMap :BANG :MINUS)]
    (if (not matched)
      (call parserMap) 
      (let [op    (previous p)
            [right p2] (unary p)]
        [(expr/makeExpr :UNARY (tokenOperator (:type op)) right) p2])))
)

(defn factor [parserMap]
  (let [[leftExpr newMap] (unary parserMap)]
    (loop [expr leftExpr
           p newMap]
      (let [[matched p2] (matchParser p :SLASH :STAR)]
        (if (not matched)
          [expr p]
            ;else 
          (let [opToken (:type (previous p2))
                op      (tokenOperator opToken)
                [right p3] (unary p2)
                newExpr (expr/makeExpr :BINARY expr op right)]
            (recur newExpr p3))))))
)

(defn term [parserMap]
  (let [[leftExpr newMap] (factor parserMap)] 
    (loop [expr leftExpr
           p newMap]
      (let [[matched p2] (matchParser p :MINUS :PLUS)]
        (if (not matched)
          [expr p]
          ;else 
          (let [opToken (:type (previous p2))
                op      (tokenOperator opToken)
                [right p3] (factor p2)
                newExpr (expr/makeExpr :BINARY expr op right)]
            (recur newExpr p3)))))
    )
)

(defn comparison [parserMap]
  (let [[leftExpr newMap] (term parserMap)] 
    (loop [expr leftExpr
           p newMap]
      (let [[matched p2] (matchParser p :GREATER :GREATER_EQUAL :LESS :LESS_EQUAL)]
        (if (not matched)
          [expr p]
          ;else 
          (let [opToken (:type (previous p2))
                op      (tokenOperator opToken)
                [right p3] (term p2)
                newExpr (expr/makeExpr :BINARY expr op right)]
            (recur newExpr p3)))))
    )
)


; Will return updated parseMaps that contain the updated current
  (defn equality [parserMap]
    ; get the left side comparison()
    (let [[leftExpr newMap] (comparison parserMap)] 
      (loop [expr leftExpr
             p    newMap]
        ;; try to match != or ==
        (let [[matched p2] (matchParser p :BANG_EQUAL :EQUAL_EQUAL)]
          (if (not matched)
            ; if nothing matched, return the expr and the parserMap
            [expr p]
            ; else, it did match so set the operator and right 
            (let [opToken (:type (previous p2))
                  op      (tokenOperator opToken)
                  [right p3] (comparison p2)
                  newExpr (expr/makeExpr :BINARY expr op right)]
              (recur newExpr p3))))))
)
  
(defn logicalAnd [parserMap]
  (loop [[expr p] (equality parserMap)]
    (let [[matched p1] (matchParser p :AND)]
      (if matched
        (let [operator (previous p1)
              [right p2] (equality p1)]
          (recur [(expr/makeExpr :LOGICAL expr operator right) p2])) ;loops with the new expr and updated parserMap
        [expr p])))
)

(defn logicalOr [parserMap]
  (loop [[expr p] (logicalAnd parserMap)]
    (let [[matched p1] (matchParser p :OR)]
      (if matched
        (let [operator (previous p1)
              [right p2] (logicalAnd p1)]
          (recur [(expr/makeExpr :LOGICAL expr operator right) p2])) ;loops with the new expr and updated parserMap
        [expr p])))
)

 (defn assignment [parserMap]
   (let [[expr p] (logicalOr parserMap)
         [matched p1] (matchParser p :EQUAL)]
     (if matched
       (let [equals (previous p1)
             [value p2] (assignment p1)]
         (cond
           (= (:type expr) :VARIABLE) (let [name (:name expr)] 
                                        [(expr/makeExpr :ASSIGN name value) p2])
 
           (= (:type expr) :GET) (let [object (:object expr) name (:name expr)] 
                                   [(expr/makeExpr :SET object name value) p2])
 
           :else
           (do
             (error equals "Invalid assignment target.") ;error message
             [expr p2]))) ;returns the expr and the updated parserMap
       ;else
       [expr p])) ;no = sign
 )


(defn expression [parserMap]
  (assignment parserMap)
)

(defn printStatement [parserMap]
  (let [[value p] (expression parserMap)
        [_ updatedParserMap] (consume p :SEMICOLON "Expect ';' after value.")] ;makes sure to update the parserMap after calling consume
    [(stmt/makeStmt :PRINT value) updatedParserMap]) ;returns the new statement and parserMap
)

;same logic as printStatement
(defn expressionStatement [parserMap]
  (let [[expr p] (expression parserMap)
        [_ updatedParserMap] (consume p :SEMICOLON "Expect ';' after expression.")]
    [(stmt/makeStmt :EXPRESSION expr) updatedParserMap])
)

(defn block [parserMap]
  (loop [p parserMap
         statements []]
    (if (and (not (check p :RIGHT_BRACE)) (not (isAtEnd p))) 
      (let [[stmt pNext] (declaration p)]
        (recur pNext (conj statements stmt))) ;continues loopint
      ; else
      (let [[_ updatedParserMap] (consume p :RIGHT_BRACE "Expect '}' after block.")]
        [statements updatedParserMap]))) ;returns statements and an updated parserMap
)

(defn function [parserMap kind]
  (let [[name pID] (consume parserMap :IDENTIFIER (str "Expect " kind " name."))
        [_ newMap] (consume pID :LEFT_PAREN (str "Expect '(' after " kind " name.")) 
        [parameters p] (if (check newMap :RIGHT_PAREN)
                          [[] newMap] ;returns the newMap if the check passes
                          ;else
                          (loop [params []
                                 p1 newMap]
                            (let [[param p2] (consume p1 :IDENTIFIER "Expect parameter name.")]
                              (when (>= (count params) 255)
                                (error (peekParser p2) "Can't have more than 255 parameters."))
                       
                              (let [[matchedComma p3] (matchParser p2 :COMMA)]
                                (if matchedComma
                                  (recur (conj params param) p3)
                                  ; else
                                  [(conj params param) p3])))))
                       
                        [_ p1] (consume p :RIGHT_PAREN "Expect ')' after parameters.")
                        [_ p2] (consume p1 :LEFT_BRACE (str "Expect '{' before " kind " body."))
                        [body p3] (block p2)]
    ; make the function statement
    [(stmt/makeStmt :FUNCTION name parameters body) p3])
)

(defn ifStatement [parserMap] 
  (let [[_ p] (consume parserMap :LEFT_PAREN "Expect '(' after 'if'.") ;consume '('
        [condition p2] (expression p)
        [_ p3] (consume p2 :RIGHT_PAREN "Expect ')' after if condition.") ;consume ')'
        [thenBranch p4] (statement p3)
        [elseBranch p5] (let [[matchedElse p6] (matchParser p4 :ELSE)]
                           (if matchedElse ;either set elseBranch to (statement parserMap) or nil 
                             (statement p6)
                             ;else 
                             [nil p6]))]  
    [(stmt/makeStmt :IF condition thenBranch elseBranch) p5]) ;return the If statement and updated parser
)

(defn whileStatement [parserMap]
  (let [[_ p] (consume parserMap :LEFT_PAREN "Expect '(' after 'while'.")
        [condition p1] (expression p)
        [_ p2] (consume p1 :RIGHT_PAREN "Expect ')' after condition.")
        [body p3] (statement p2)]
    [(stmt/makeStmt :WHILE condition body) p3])
)

(declare varDeclaration)

(defn forStatement [parserMap]
  (let [[_ p] (consume parserMap :LEFT_PAREN "Expect '(' after 'for'.") 
        [matchedS pS] (matchParser p :SEMICOLON)
        [initializer p1] (if matchedS
                           [nil pS] ;sets initializer to nil
                           ;else
                           (let [[matchedV pV] (matchParser p :VAR)]
                             (if matchedV
                               (varDeclaration pV)
                               ;else
                               (expressionStatement p))))
 
        [condition p2] (if (not (check p1 :SEMICOLON))
                         (expression p1)
                         ;else
                         [nil p1])
        
        [_ p3] (consume p2 :SEMICOLON "Expect ';' after loop condition.")
        
        [increment p4] (if (not (check p3 :RIGHT_PAREN))
                         (expression p3)
                         ;else
                         [nil p3])
        
        [_ p5] (consume p4 :RIGHT_PAREN "Expect ')' after for clauses.")

        [body p6] (statement p5)

        bodyStmts (if (= (:type body) :BLOCK)
                    (:statements body)
                    ;else
                    [body])

        bodyWithIncrement (if increment
                            (conj bodyStmts (stmt/makeStmt :EXPRESSION increment))
                            ;else
                            bodyStmts)
        
        whileLoop (stmt/makeStmt :WHILE (or condition (expr/makeExpr :LITERAL true))
                                 (stmt/makeStmt :BLOCK bodyWithIncrement))
        
        fullBody (if initializer
                   (stmt/makeStmt :BLOCK [initializer whileLoop])
                   ;else
                   whileLoop)]
    [fullBody p6]))

(defn returnStatement [parserMap]
  (let [keyword (previous parserMap)
        [value p] (if (not (check parserMap :SEMICOLON))
                     (expression parserMap)
                    ;else set it to nil, and pass back the orginal parserMap
                     [nil parserMap])
        [_ p1] (consume p :SEMICOLON "Expect ';' after return value.")]
    [(stmt/makeStmt :RETURN keyword value) p1])
)

(defn statement [parserMap]
  (let [[matchedPR pPR] (matchParser parserMap :PRINT)
        [matchedLB pLB] (matchParser parserMap :LEFT_BRACE)
        [matchedIf pIf] (matchParser parserMap :IF)
        [matchedR pR]  (matchParser parserMap :RETURN)
        [matchedW pW]  (matchParser parserMap :WHILE)
        [matchedF pF]  (matchParser parserMap :FOR)]
    (cond
      matchedPR (printStatement pPR)
      matchedLB (let [[stmts p] (block pLB)]
                  [(stmt/makeStmt :BLOCK stmts) p])
      matchedIf (ifStatement pIf)
      matchedR  (returnStatement pR)
      matchedW  (whileStatement pW)
      matchedF  (forStatement pF)

      :else     (expressionStatement parserMap)))
)



(defn varDeclaration [parserMap]
  (let [[name p1] (consume parserMap :IDENTIFIER "Expect variable name.")
        [matched p2] (matchParser p1 :EQUAL)
        [initializer p3] (if matched
                           (expression p2)
                           ;else
                           [nil p2])
        [_ p4] (consume p3 :SEMICOLON "Expect ';' after variable declaration.")
        stmt (stmt/makeStmt :VAR name initializer)]
    [stmt p4])
)

(defn classDeclaration [parserMap]
  (let [[name p1] (consume parserMap :IDENTIFIER "Expect class name.")
        
        [matched newMap] (matchParser p1 :LESS)
        
        [superclass p2] (if matched
                          (let [[_ innerMap] (consume newMap :IDENTIFIER "Expect superclass name.")
                                scExpr (expr/makeExpr :VARIABLE (previous innerMap))]
                            [scExpr innerMap]) 
                          ;else superclass is nil
                          [nil newMap])
        
        [_ p3] (consume p2 :LEFT_BRACE "Expect '{' before class body.")

        [methods p4] (loop [mthds [] ;empty vector
                            p p3]
                       (if (and (not (check p :RIGHT_BRACE)) (not (isAtEnd p)))
                         (let [[method p-next] (function p "method")]
                           (recur (conj mthds method) p-next)) ;loops again
                         ;else returns methods and the updatedMap
                         [mthds p]))
        
        [_ p5] (consume p4 :RIGHT_BRACE "Expect '}' after class body.")

        stmt (stmt/makeStmt :CLASS name superclass methods)]
    [stmt p5]) ;returns the new stmt and updatedMap
)

(defn declaration [parserMap]
  (try
    (let [[matchedFun pFun] (matchParser parserMap :FUN)
          [matchedVar pVar] (matchParser parserMap :VAR)
          [matchedClass pClass] (matchParser parserMap :CLASS)]
      (cond
        matchedFun   (function pFun "function")
        matchedVar   (varDeclaration pVar)
        matchedClass (classDeclaration pClass)
        :else        (statement parserMap)))
    (catch clojure.lang.ExceptionInfo _e
      [(synchronize parserMap) nil]))
)