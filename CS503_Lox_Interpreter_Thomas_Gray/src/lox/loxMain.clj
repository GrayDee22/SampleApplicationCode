(ns lox.loxMain
  (:require
   [lox.environment :as env]
   [lox.error :as error]
   [lox.interpreter :as interpreter]
   [lox.parser :as parser]
   [lox.resolver :as resolver]
   [lox.runtimeError :as runTime]
   [lox.scanner :as scanner]))

(def globalEnv
  (let [env (env/makeEnvironment nil)]
    (env/define env "clock" (env/makeClock)) ;adds the clock to the globalEnv
    env)
)

(defn run [input]
  (let [tokens    (scanner/scanTokens input)
        parser    (parser/makeParser tokens)
        statements (parser/parse parser)]  ; returns stmt or nil
    (when statements
      (let [resolver (resolver/resolver interpreter/Interpreter)]
        ; resolve all statements
        (resolver/resolveStatements resolver statements))
      ; only interprets there was no error 
      (when (not @error/hadError)
        (interpreter/interpret statements globalEnv))))
)


(defn balancedBraces? [s]
  (loop [chars (seq s)
         opens 0
         closes 0]
    (if (empty? chars) ;if no more input ends the loop
      (= opens closes) ; serves as a check, to be used in a if stmt (ex: if balancedBraces? is true do ..., else do ...)
      ;else
      (let [c (first chars)]
        (cond
          (= c \{) (recur (rest chars) (inc opens) closes)
          (= c \}) (recur (rest chars) opens (inc closes))
          :else    (recur (rest chars) opens closes)))))
)

; Read file, skip empty lines and comments
(defn readInFile [file]
  (let [lines (seq (.split (slurp file) "\n"))]
    (loop [buffer "" remaining-lines lines]
      (if (empty? remaining-lines)
        (when (seq buffer)
          (run buffer))
        ;else 
        (let [line (first remaining-lines)
              newBuffer (if (empty? buffer) 
                          line ;passes line
                          ;else
                          (str buffer "\n" line))]
          (if (balancedBraces? newBuffer)
            (do
              (run newBuffer)
              (recur "" (rest remaining-lines)))
            ;else
            (recur newBuffer (rest remaining-lines)))))))
  (when @runTime/hadRuntimeError
    (System/exit 70))
)

; REPL
(defn repl []
  (println "Lox REPL (type 'exit' to quit)")
  (loop [buffer ""] ;loops until user types exit
    (when (empty? buffer)
      (print "> ") (flush))
    (let [line (read-line)]  ;reads a line of user input
      (cond
        (or (nil? line) (= line "exit")) (println "Goodbye!") ;exits the repl if line = exit or nil

        :else (let [newBuffer (if (empty? buffer) 
                                line ;return the line
                                ;else
                                (str buffer "\n" line))] 
                (if (balancedBraces? newBuffer) 
                  (do 
                    (try 
                      (run newBuffer) 
                      (catch clojure.lang.ExceptionInfo e 
                        (runTime/runtimeError (.getMessage e))))
                    (recur "")) ;reset buffer 
                  ;else
                  (do
                    ;prints ... to show that you are in brackets 
                    (print "... > ") (flush) 
                    (recur newBuffer))))))) ; repeats the loop
)

;main
(defn -main [& args] (cond
    (> (count args) 1)  ; if there are more that 1 args
    (do
      (println "Too many arguments")
      (System/exit 1)) 

    (= (count args) 1) (readInFile (first args)) ; if args = 1, reads in the file
                       
    :else (repl)) ;else starts the repl
)