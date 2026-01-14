(ns lox.scanner 
  (:require [lox.error :refer [error]])
)
; Set of all token types
(def TokenType
  #{;single-character tokens
    :LEFT_PAREN :RIGHT_PAREN
    :LEFT_BRACE :RIGHT_BRACE
    :COMMA :DOT :MINUS :PLUS :SEMICOLON :SLASH :STAR

    ;one or two character tokens
    :BANG :BANG_EQUAL
    :EQUAL :EQUAL_EQUAL
    :GREATER :GREATER_EQUAL
    :LESS :LESS_EQUAL

    ;literals
    :IDENTIFIER :STRING :NUMBER

    ;keywords
    :AND :CLASS :ELSE :FALSE :FUN :FOR :IF :NIL :OR
    :PRINT :RETURN :SUPER :THIS :TRUE :VAR :WHILE
    
    :EOF
    }
)

(defn Token [type lexeme literal line] {
   :type type
   :lexeme lexeme ;chars in order that produced the token
   :literal literal ;stores the value associated with the token
   :line line
  }
)

;(def p (Token (TokenType :type ...)))

(defn toString [token]
  (str (:type token) " " (:lexeme token) " " (:literal token))
)

(defn Scanner [source]{
   :source source ;code to be scanned
   :tokens []     ;empty vector to store the scanned tokens                
})

;Returns the char at the current index, and updates current by 1
(defn advance [source current]
  [(nth source current) (inc current)]
) 

;adds the token to list of tokens
(defn addToken [tokens type lexeme literal line]
  (conj tokens (Token type lexeme literal line)) 
)

;similar to advance, but only grabs the next character if it matches a specific one
(defn match [source current expected]
  ;checks if we haven't reached the end of the source and its the char thats expected
  (if (and (< current (count source)) (= (nth source current) expected))
     [(inc current) true]  ; if so increment current and return true 
     ;else 
     [current false]) ;return false
)

;helper for tokens that can optionally have an '='
(defn addOptionalEqualToken [tokens char current line baseToken equalToken source]
  (let [[newCurrent matched] (match source current \=)] ;matched returns true or false if its the '=' token we are looking for
    ; add the new token to the tokens vector
    [(addToken tokens (if matched ;chose which token it is
                        equalToken ;if matched returned true
                        baseToken) ;else if returned false
                      (str char (when matched "=")) nil line) ;when matched is true add the = sign to the orginal char, else just keep the orginal char no = sign
     newCurrent line]) ;returns the updated index and the line
)

(defn peekChar [current source]
  (if (< current (count source)) ;if the current is less than the length of the source
     (nth source current) ;returns the character where the current idx is
    ;else return null char if at end
     \0)
)

;Helper function to handle / that can be for comments or divison
(defn commentOrDiv [tokens current line source] 
  (let [[newCurrent isComment] (match source current \/)] ;check if the next char is also a '/' using match
    (if isComment ;If so it is a comment 
      (loop [idx newCurrent] ;set idx to the newCurrent index, loop to skip until end of the line
        (if (or (>= idx (count source)) (= (peekChar idx source) \newline))
          [tokens idx line] ; returns tokens, & the updated idx at the end of comment
          (recur (inc idx)))) ;increments the index
      ; else, just a division token, so return the updated tokens vector and new idx
      [(addToken tokens :SLASH "/" nil line) newCurrent line]))
)

;Helper function to handle strings
(defn string [tokens start current line source]
  ; Loop through characters closing " is found, or the end of source
  (loop [idx current
         ln line]
    (cond
      ; ran out of input
      (>= idx (count source)) (do
                                (error ln "Unterminated string.") ; report the error through the error function
                                [tokens idx ln])

      ; newline inside string, increments the line count
      (= (peekChar idx source) \newline) (recur (inc idx) (inc ln))

      ; closing quote found
      (= (peekChar idx source) \") (let
                                 [stringValue (subs source (inc start) idx)] ; use subs, similar to the books substring function to get the string value
                                 [(addToken tokens :STRING stringValue stringValue ln) (inc idx) ln]) ; add to tokens & update the idx past the closing quote

      ; Any other character: keep looping
      :else
      (recur (inc idx) ln)))
)

;Looks at the next char
(defn peekNext [current source]
  (if (>= (inc current) (count source))
    ;if current++ is >= than the source return null char
    \0
    (nth source (inc current))) ;get the next char
)

;Checks if the char is a number
(defn isDigit [char]
  (and (>= (int char) (int \0)) (<= (int char) (int \9))) ;the char/number is between 0-9
)

;Helper function for numbers
(defn number [tokens start current line source]
  ; loop to consume integer part
  (let [idx (loop [i current]
              (if (and (< i (count source)) (isDigit (peekChar i source)))
                (recur (second (advance source i))) ;use second like cdr to just grab the updated idx
                i)) ; else return the updated index
        
        ; sets the finalIdx to the idx after the number after parsing through the number
        finalIdx (if (and (< idx (count source))
                          (= (peekChar idx source) \.)
                          (isDigit (peekNext idx source)))
                   ; if current char is a dot, and the next char is a number create a new idx that is located after the dot
                   (loop [newIdx (second (advance source idx))]
                     (if (and (< newIdx (count source)) (isDigit (peekChar newIdx source)))
                       ; keep looping
                       (recur (second (advance source newIdx)))
                       ; else return the new index
                       newIdx))
                   ; else keep current index
                   idx)
        lexeme (subs source start finalIdx)
        value  (Double/parseDouble lexeme)]
    ; Add token and return updated index and line
    [(addToken tokens :NUMBER lexeme value line) finalIdx line])
)


; Checks if char is a letter or underscore
(defn isAlpha [c]
  (or
   (and (>= (int c) (int \a)) (<= (int c) (int \z))) ; a-z
   (and (>= (int c) (int \A)) (<= (int c) (int \Z))) ; A-Z
   (= c \_)) ; underscore
)

; Checks if char is a letter, underscore, or digit
(defn isAlphaNumeric [c]
  (or (isAlpha c) (isDigit c))
)

;Keywords map
(def keywords
  {"and"    :AND
   "class"  :CLASS
   "else"   :ELSE
   "false"  :FALSE
   "for"    :FOR
   "fun"    :FUN
   "if"     :IF
   "nil"    :NIL
   "or"     :OR
   "print"  :PRINT
   "return" :RETURN
   "super"  :SUPER
   "this"   :THIS
   "true"   :TRUE
   "var"    :VAR
   "while"  :WHILE
   }
)

(defn identifier [tokens start current line source]
  ; loops through letters and digits
  (let [idx (loop [i current]
              (if (and (< i (count source)) (isAlphaNumeric (peekChar i source)))
                ;if the next char is a number or letter loop again
                (recur (second (advance source i)))
                ;else no longer alphaNumeric so just return the current index
                i))
        lexeme (subs source start idx) ;grabs the text(lexeme) between the starting index and the udated idx from the loop
        tokenType (get keywords lexeme :IDENTIFIER)] ;passes the text into the keywords map, and pulls the token type if it's in the map
    [(addToken tokens tokenType lexeme nil line) idx line]) ;adds that token to the tokens vector, and returns the updated index
)


(defn scanToken [start current line tokens source] 
  (let [[char newCurrent] (advance source current)] ; grab the current char and increment
    (cond
   ; single-character tokens                                                
      (= char \() [(addToken tokens :LEFT_PAREN "(" nil line) newCurrent line]
      (= char \)) [(addToken tokens :RIGHT_PAREN ")" nil line) newCurrent line]
      (= char \{) [(addToken tokens :LEFT_BRACE "{" nil line) newCurrent line]
      (= char \}) [(addToken tokens :RIGHT_BRACE "}" nil line) newCurrent line]
      (= char \,) [(addToken tokens :COMMA "," nil line) newCurrent line]
      (= char \.) [(addToken tokens :DOT "." nil line) newCurrent line]
      (= char \-) [(addToken tokens :MINUS "-" nil line) newCurrent line]
      (= char \+) [(addToken tokens :PLUS "+" nil line) newCurrent line]
      (= char \;) [(addToken tokens :SEMICOLON ";" nil line) newCurrent line]
      (= char \*) [(addToken tokens :STAR "*" nil line) newCurrent line]
      
   ; two-character tokens                                               
      (= char \!) (addOptionalEqualToken tokens char newCurrent line :BANG :BANG_EQUAL source)
      (= char \=) (addOptionalEqualToken tokens char newCurrent line :EQUAL :EQUAL_EQUAL source)
      (= char \<) (addOptionalEqualToken tokens char newCurrent line :LESS :LESS_EQUAL source)
      (= char \>) (addOptionalEqualToken tokens char newCurrent line :GREATER :GREATER_EQUAL source)     
      
   ; comments or divison
      (= char \/) (commentOrDiv tokens newCurrent line source)
      
   ; spaces, tabs, returns, and newlines
      (or (= char \space) (= char \return) (= char \tab)) [tokens newCurrent line] ;ignores
      (= char \newline) [tokens newCurrent (inc line)] ; increment line if its a newline
      
   ; strings
      (= char \") (string tokens start newCurrent line source) ;pass it newCurrent as it skips the opening quote

   ; numbers
      (isDigit char) (number tokens start current line source)

   ; identifiers / keywords
      (isAlpha char) (identifier tokens start current line source)

      :else (do
              (error line "Unexpected character.")
              [tokens newCurrent line]) ;reports the error & returns tokens current and line so scanning can continue  
      ))
)


(defn scanTokens [source]
  (loop [start 0        ; start idx of the current lexeme
         current 0      ; current idx we're looking at
         line 1         ; current line number
         tokens []]     ; scanned tokens so far
    (if (>= current (count source)) 
      (conj tokens (Token :EOF "" nil line)) ; append EOF token when finished scanning
      ; else, scan the next token
      (let [[newTokens newCurrent newLine] (scanToken start current line tokens source)] 
        ; Set start = current for the next token
        (recur newCurrent newCurrent newLine newTokens))))
)
