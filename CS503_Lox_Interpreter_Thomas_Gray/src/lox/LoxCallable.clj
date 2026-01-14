(ns lox.LoxCallable)

(defprotocol LoxCallable
  (call [this interpreter arguments])
  (arity [this])
)