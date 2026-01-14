(ns lox.LoxClass
  (:require [lox.LoxInstance :refer [makeLoxInstance]]))


(defn makeLoxClass [name superclass methods]
  {:name name
   :methods methods
   :superclass superclass

   :call (fn [this interpreter arguments] (let [instance (makeLoxInstance this) 
                                                initializer ((:findMethod this) this "init")] 
                                            (when initializer
                                              ; binds 'this', and calls the initializer with arguments 
                                              ((:call ((:bind initializer) initializer instance)) this interpreter arguments)) 
                                            instance)) 

   :arity (fn [_this] (let [initializer ((:findMethod _this) _this "init")] 
                        (if (nil? initializer)
                          0 ;if nill return 0
                          ;else
                          ((:arity initializer) initializer) 
                          )))

   :toString (fn [] name) 

   :findMethod (fn [_klass methodName] (let [method (get (:methods _klass) methodName) 
                                             superclass  (:superclass _klass)] 
                                         (if method 
                                           method ;returns method
                                           ;else 
                                           (when superclass ;when not nil
                                             ((:findMethod superclass) superclass methodName))))) ;finds the superclass
   }
)